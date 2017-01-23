package mars.interfaces;

import mars.exceptions.DisallowedRoverMovementException;
import mars.valueobjects.GridLocation;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * The interface needed by all subclasses that intend to enforce
 * some kind of movement of the rover
 */
public interface IMovementEnforcer extends IEnforcer{
    public GridLocation moveRover(GridLocation gridLocation) throws DisallowedRoverMovementException;
}
