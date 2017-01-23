package mars.exceptions;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This exception is thrown when there is no initial
 * landing position given in the input from Nasa.
 */
public class MissingInitialLandingPositionException extends LandingPositionException {

    public MissingInitialLandingPositionException(String message){
        super(message);
    }
}
