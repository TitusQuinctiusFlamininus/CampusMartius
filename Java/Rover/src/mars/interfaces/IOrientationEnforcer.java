package mars.interfaces;

import mars.valueobjects.GridLocation;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * The interface needed by all subclasses that intend to enforce
 * some kind of orientation of the rover
 */
public interface IOrientationEnforcer extends IEnforcer{
    public GridLocation orientRover(GridLocation gridLocation, String orientationInstruction);
}
