package mars.validators;

import mars.constants.NASAInputConstraints;
import mars.exceptions.InitialLandingPositionException;
import mars.exceptions.MissingInitialLandingPositionException;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This class checks that we have a valid Landing Position on which to place
 * the Rover on Mars. From that point on, only then can we deal with movement and
 * orientation instructions, also sent by NASA
 */
public class LandingPositionInputValidator {

    public boolean validateInitialLandingPosition(String inputLandingPosition) throws InitialLandingPositionException, MissingInitialLandingPositionException{
        if(null==inputLandingPosition){
            throw new MissingInitialLandingPositionException("The Rover's Initial Landing Position is Completely Missing! Houston, we have a huge problem!");
        }
        Matcher match = Pattern.compile(NASAInputConstraints.LANDING_POSITION_INPUT_STANDARD_REGEX.VALUE()).matcher(inputLandingPosition);
        if(!match.find()) {
            throw new InitialLandingPositionException("The Input String for the Rover's Initial Landing Position is not NASA standard: It should conform to the following Regular Expression: "
                    + NASAInputConstraints.LANDING_POSITION_INPUT_STANDARD_REGEX.VALUE());
        }
        return true;
    }
}
