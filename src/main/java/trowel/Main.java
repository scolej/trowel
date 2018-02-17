package trowel;

import static java.util.stream.Collectors.toList;

import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.logging.LogManager;
import java.util.logging.Logger;
import java.util.regex.Matcher;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import fi.iki.elonen.NanoHTTPD;
import trowel.json.Match;
import trowel.json.Reply;
import trowel.json.Request;

public class Main extends NanoHTTPD {

	private static final Logger LOGGER = Logger.getLogger(Main.class.getName());

	public static void main(final String[] args) throws IOException {
		LogManager.getLogManager().readConfiguration(ClassLoader.class.getResourceAsStream("/logging.properties"));
		printStartupBuildInfo();
		new Main();
	}

	private static void printStartupBuildInfo() {
		final Properties prop = new Properties();

		try (InputStream in = ClassLoader.class.getResourceAsStream("/build.properties")) {
			prop.load(in);
		} catch (final IOException | NullPointerException e) {
			e.printStackTrace();
			Thread.interrupted();
		}

		LOGGER.info(String.join(System.lineSeparator(), "Starting Trowel!",
				"Built at: " + prop.getProperty("buildDate", "?"),
				"From commit: " + prop.getProperty("commitHash", "?")));
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
			LOGGER.info("Received: " + requestJson);

			final Gson gson = new GsonBuilder().create();
			final Request request = gson.fromJson(requestJson, Request.class);

			final Reply reply;
			if ("lookup".equals(request.action)) {

				final Path searchPath = Paths.get(".").toAbsolutePath().normalize();
				final List<StepDefinition> glue = StepFinder.findStepsInTree(searchPath).collect(toList());
				LOGGER.info("Found " + glue.size() + " definitions under directory " + searchPath);

				final List<StepDefinition> matches = glue.stream().filter(g -> {
					final Matcher m = g.pattern().matcher(request.stepText);
					return m.matches();
				}).collect(toList());

				reply = new Reply(matches.stream().map(Match::new).collect(toList()));

			} else {
				reply = new Reply(Arrays.asList());
			}

			final String json = gson.toJson(reply);
			LOGGER.info("Replied " + ": " + json);
			return newFixedLengthResponse(json + "\n");

		} catch (final IOException | ResponseException e) {
			e.printStackTrace();
			return newFixedLengthResponse("Broken :(");
		}

	}
}
