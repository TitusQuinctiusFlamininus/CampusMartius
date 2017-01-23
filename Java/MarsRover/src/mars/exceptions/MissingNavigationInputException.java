package mars.exceptions;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This exception is thrown when there is no navigational
 * or movement information provided by NASA
 */
public class MissingNavigationInputException extends NavigationException {

    public MissingNavigationInputException(String message){
        super(message);
    }
}
