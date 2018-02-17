public final class Glue {
    @Given("I have a thing.")
    public void glue1() {
    }
    @When("I (do|give|have) a thing.")    
    public void glue2() {
    }
	@Then("\\(parens for trouble\\)")
    public void glue3() {
    }
    @Given("\"quotes for trouble\"")
    public void glue4() {
    }
    @When("\\\\backslashes for trouble\\\\")
    public void glue5() {
    }
    @Then("the dog is on the (.*)")
    public void glue6() {
    }
    @Given("the cat is on the ([A-Z]+)")
    public void glue7() {
    }
    @Given("the regex is broken ([A-Z+)")
    public void glue8() {
    }
}
