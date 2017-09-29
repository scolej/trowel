package trowel;

import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.stream.Stream;

import fi.iki.elonen.NanoHTTPD;

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
			final String request = map.get("postData");

			System.out.println("Received " + ": [" + request + "]");

			Stream<StepDefinition> glue;
			glue = StepFinder.findStepsInTree(Paths.get("."));
			final List<StepDefinition> matches = glue.filter(g -> {
				final Matcher m = g.pattern().matcher(request);
				return m.matches();
			}).collect(toList());

			final String reply = matches.stream().map(m -> m.file() + ":" + m.lineNumber()).collect(joining());
			System.out.println("Replied " + ": [" + reply + "]");

			return newFixedLengthResponse(reply + "\n");

		} catch (final IOException | ResponseException e) {
			e.printStackTrace();
			return newFixedLengthResponse("Broken :(");
		}

	}
}
