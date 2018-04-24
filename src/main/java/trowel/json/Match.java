package trowel.json;

import trowel.StepDefinition;

public class Match {
  public String file;
  public int lineNumber;

  public Match(final StepDefinition def) {
    this(def.file().toString(), def.lineNumber());
  }

  private Match(final String p, final int l) {
    file = p;
    lineNumber = l;
  }
}
