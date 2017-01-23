package mars.exceptions;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This exception is a general Landing Exception class
 * It has two subclasses that deal move concretely with specific
 * landing instruction exceptions
 */
public class LandingPositionException extends Exception{
    public LandingPositionException(String message){
        super(message);
    }
}
