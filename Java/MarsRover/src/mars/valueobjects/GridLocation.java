package mars.valueobjects;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This represents a grid location in its entirety. There are 3
 * basic properties to a grid location:
 * X-Position, Y-Position and the Orientation
 */
public class GridLocation {
    private int Current_X_position;
    private int Current_Y_position;
    private String Current_CardinalOrientation;

    public int getCurrent_X_position() {
        return Current_X_position;
    }

    public void setCurrent_X_position(int current_X_position) {
        Current_X_position = current_X_position;
    }

    public int getCurrent_Y_position() {
        return Current_Y_position;
    }

    public void setCurrent_Y_position(int current_Y_position) {
        Current_Y_position = current_Y_position;
    }

    public String getCurrent_CardinalOrientation() {
        return Current_CardinalOrientation;
    }

    public void setCurrent_CardinalOrientation(String current_CardinalOrientation) {
        Current_CardinalOrientation = current_CardinalOrientation;
    }
}
