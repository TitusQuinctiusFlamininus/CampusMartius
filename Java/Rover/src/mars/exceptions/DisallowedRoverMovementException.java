package mars.exceptions;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This exception is thrown when movement is suggested
 * that goes outside of the boundaries of mars surface space
 *
 */
public class DisallowedRoverMovementException extends Exception{

    public DisallowedRoverMovementException(String message){
        super(message);
    }
}
