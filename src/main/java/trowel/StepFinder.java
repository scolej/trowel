package trowel;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

public class StepFinder {
	private static final Pattern GLUE_ANNOTATION_PATTERN = Pattern.compile("@(Given|When|Then)\\(\".*\"\\)");

	public static final Stream<StepDefinition> findStepsInLines(final Path path, final Iterable<String> lines) {
		final List<StepDefinition> results = new ArrayList<>();
		int lineNumber = 1;
		for (final String l : lines) {
			final Matcher matcher = GLUE_ANNOTATION_PATTERN.matcher(l);
			if (matcher.matches()) {
				results.add(new StepDefinition(matcher.group(2), path, lineNumber));
			}
			lineNumber += 1;
		}
		return results.stream();
	}

	public static final Stream<StepDefinition> findStepsInTree(final Path root) throws IOException {

		final Stream<Path> javaFiles = Files.walk(root).filter(Files::isRegularFile)
				.filter(f -> f.toString().endsWith(".java"));
		return javaFiles.flatMap(path -> {
			try (Stream<String> lines = Files.lines(path)) {
				return findStepsInLines(path, lines::iterator);
			} catch (final IOException e) {
				e.printStackTrace();
				/* FIXME Make me more total. */
				return Stream.empty();
			}
		});

	}
}
