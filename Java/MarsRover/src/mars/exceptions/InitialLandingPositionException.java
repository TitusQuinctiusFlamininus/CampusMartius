package mars.exceptions;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This exception if thrown when the initial landing position
 * does not makes sense with respect to the map of the designated
 * area that the rover can initially place itself before carrying out any
 * instruction
 */
public class InitialLandingPositionException extends LandingPositionException {

    public InitialLandingPositionException(String message){
        super(message);
    }
}
