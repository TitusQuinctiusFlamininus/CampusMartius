package mars.constants;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * These are regular expressions that help the
 * validators find out if NASA input really came from NASA
 * and is a valid movement or orientation command
 */
public enum NASAInputConstraints {

    //Landing-Position Instructions must conform to this regex in order to be processed
    LANDING_POSITION_INPUT_STANDARD_REGEX("^\\d{2}[NSEW]$"),

    //Navigating Instructions must conform to this regex in order to be processed
    //ASSUMPTION: It is quite relaxed on legitimate L,R,M mixed among other wierd character and symbols
    NAVIGATION_INPUT_STANDARD_REGEX("[LRM]{1,}");

    private String value;
    private NASAInputConstraints(String thevalue){
        this.value = thevalue;
    }

    public String VALUE(){
        return this.value;
    }
}
