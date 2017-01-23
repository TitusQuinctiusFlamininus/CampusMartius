package mars.exceptions;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This exception is thrown when navigation is received from NASA
 * but there is nothing in the instructions that makes any sense to
 * constitute even a single basic movement by the rover.
 */
public class NavigationInputException extends NavigationException {
    public NavigationInputException(String message){
        super(message);
    }
}
