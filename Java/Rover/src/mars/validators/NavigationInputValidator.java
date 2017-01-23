package mars.validators;

import mars.constants.NASAInputConstraints;
import mars.exceptions.MissingNavigationInputException;
import mars.exceptions.NavigationInputException;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This class validates the Movement instructions received from Nasa: There has to be
 * a certain standard for the rover to make any basic movements
 */
public class NavigationInputValidator {
    public boolean validateNavigationalInput(String navigationInput) throws Exception{
        if(null == navigationInput){
            throw new MissingNavigationInputException("The Rover's Navigation Instructions are completely missing! Houston, we have landed but we cannot distribute candies!");
        }
        Matcher match = Pattern.compile(NASAInputConstraints.NAVIGATION_INPUT_STANDARD_REGEX.VALUE()).matcher(navigationInput);
        if(!match.find()) {
            throw new NavigationInputException("The Input String for the Rover's Navigation Instructions is not NASA standard: It should conform to the following Regular Expression: "
                    + NASAInputConstraints.NAVIGATION_INPUT_STANDARD_REGEX.VALUE());
        }
        return true;
    }
}
