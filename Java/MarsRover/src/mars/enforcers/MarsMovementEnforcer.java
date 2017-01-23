package mars.enforcers;

import mars.constants.CardinalDirections;
import mars.constants.MarsPlanetaryConstraints;
import mars.interfaces.IMovementEnforcer;
import mars.valueobjects.GridLocation;
import mars.exceptions.DisallowedRoverMovementException;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This class is responsible for actually moving the Rover.
 * The X and Y positions of the rover are calculated and the rover actually moved
 */
public class MarsMovementEnforcer implements IMovementEnforcer{
    public GridLocation moveRover(GridLocation gridLocation) throws DisallowedRoverMovementException{

        if (gridLocation.getCurrent_CardinalOrientation().equals(CardinalDirections.NORTH.DIR())) {
            if((gridLocation.getCurrent_Y_position() == MarsPlanetaryConstraints.MAX_GRID_INDEX.VALUE())){
                throw new DisallowedRoverMovementException("The Mars Rover is Not Allowed further NORTH than the Designated Roving Area: Sure the Instructions came from NASA?");
            }
            move_NORTH(gridLocation);
            return gridLocation;
        }
        else if (gridLocation.getCurrent_CardinalOrientation().equals(CardinalDirections.EAST.DIR())) {
            if((gridLocation.getCurrent_X_position() == MarsPlanetaryConstraints.MAX_GRID_INDEX.VALUE())){
                throw new DisallowedRoverMovementException("The Mars Rover is Not Allowed further EAST than the Designated Roving Area: Sure the Instructions came from NASA?");
            }
            move_EAST(gridLocation);
            return gridLocation;
        }
        else if (gridLocation.getCurrent_CardinalOrientation().equals(CardinalDirections.SOUTH.DIR())) {
            if((gridLocation.getCurrent_Y_position() == MarsPlanetaryConstraints.MIN_GRID_INDEX.VALUE())){
                throw new DisallowedRoverMovementException("The Mars Rover is Not Allowed further SOUTH than the Designated Roving Area: Sure the Instructions came from NASA?");
            }
            move_SOUTH(gridLocation);
            return gridLocation;
        }
        else if (gridLocation.getCurrent_CardinalOrientation().equals(CardinalDirections.WEST.DIR())) {
            if((gridLocation.getCurrent_X_position() == MarsPlanetaryConstraints.MIN_GRID_INDEX.VALUE())){
                throw new DisallowedRoverMovementException("The Mars Rover is Not Allowed further WEST than the Designated Roving Area: Sure the Instructions came from NASA?");
            }
            move_WEST(gridLocation);
            return gridLocation;
        }
        return gridLocation;
    }

    private void move_WEST(GridLocation gridLocation) {
        gridLocation.setCurrent_X_position(gridLocation.getCurrent_X_position() - 1);
    }

    private void move_SOUTH(GridLocation gridLocation) {
        gridLocation.setCurrent_Y_position(gridLocation.getCurrent_Y_position() - 1);
    }

    private void move_EAST(GridLocation gridLocation) {
        gridLocation.setCurrent_X_position(gridLocation.getCurrent_X_position() + 1);
    }

    private void move_NORTH(GridLocation gridLocation) {
        gridLocation.setCurrent_Y_position(gridLocation.getCurrent_Y_position() + 1);
    }
}
