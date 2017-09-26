package trowel;

import static java.util.stream.Collectors.toList;

import java.io.IOException;
import java.nio.file.Paths;
import java.util.List;
import java.util.regex.Matcher;
import java.util.stream.Stream;

public class Main {
	public static void main(final String[] args) throws IOException {

		System.out.println(System.getProperty("user.dir"));

		final Stream<StepDefinition> glue = StepFinder.findStepsInTree(Paths.get(args[0]));

		final String input = "I have a thing.";

		final List<StepDefinition> matches = glue.filter(g -> {
			final Matcher m = g.pattern().matcher(input);
			return m.matches();
		}).collect(toList());

		return;
	}
}
