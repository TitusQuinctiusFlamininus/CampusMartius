package mars.exceptions;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This exception is a general Navigation Exception class
 * It has two subclasses that deal move concretely with specific
 * movement instruction exceptions
 */
public class NavigationException extends Exception {
    public NavigationException(String message){
        super(message);
    }
}
