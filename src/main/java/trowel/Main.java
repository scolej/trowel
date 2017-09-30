package trowel;

import static java.util.stream.Collectors.toList;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.stream.Stream;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import fi.iki.elonen.NanoHTTPD;
import trowel.json.Match;
import trowel.json.Reply;
import trowel.json.Request;

public class Main extends NanoHTTPD {

	public static void main(final String[] args) throws IOException {
		new Main();
	}

	public Main() throws IOException {
		super(5555);
		start(NanoHTTPD.SOCKET_READ_TIMEOUT, false);
	}

	@Override
	public Response serve(final IHTTPSession session) {
		try {

			final Map<String, String> map = new HashMap<String, String>();
			session.parseBody(map);
			final String requestJson = map.get("postData");
			System.out.println("Received: " + requestJson);

			final Gson gson = new GsonBuilder().create();
			final Request request = gson.fromJson(requestJson, Request.class);

			final Reply reply;
			if ("lookup".equals(request.action)) {

				final Stream<StepDefinition> glue = StepFinder.findStepsInTree(Paths.get("."));
				final List<StepDefinition> matches = glue.filter(g -> {
					final Matcher m = g.pattern().matcher(request.stepText);
					return m.matches();
				}).collect(toList());

				reply = new Reply(matches.stream().map(Match::new).collect(toList()));

			} else {
				reply = new Reply(Arrays.asList());
			}

			final String json = gson.toJson(reply);
			System.out.println("Replied " + ": " + json);
			return newFixedLengthResponse(json + "\n");

		} catch (final IOException | ResponseException e) {
			e.printStackTrace();
			return newFixedLengthResponse("Broken :(");
		}

	}
}
