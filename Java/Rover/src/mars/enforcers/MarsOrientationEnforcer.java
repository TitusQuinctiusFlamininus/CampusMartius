package mars.enforcers;

import mars.builders.OrientationMapBuilder;
import mars.constants.CardinalDirections;
import mars.interfaces.IOrientationEnforcer;
import mars.valueobjects.GridLocation;
import mars.constants.OrientationInstruction;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This class is responsible orienting the rover
 * correctly in its current position before a possible (but not necessary)
 * physical movement. Since direction determines the values of X and Y positions
 * given a needed "M" movement, it is important to make sure this Enforcer is solid
 * in rule enforcement
 */

public class MarsOrientationEnforcer implements IOrientationEnforcer{

    private HashMap<String, Map<String,String>> orientationMap;

    public MarsOrientationEnforcer(){
          orientationMap = new OrientationMapBuilder().getOrientationMap();
    }

    public GridLocation orientRover(GridLocation gridLocation, String orientationInstruction) {
        if(orientationInstruction.equals(OrientationInstruction.LEFT.way()) || orientationInstruction.equals(OrientationInstruction.RIGHT.way())) {
            Map<String, String> directionMap = find_Correct_Direction_Map(gridLocation);
            gridLocation.setCurrent_CardinalOrientation(find_NewOrientation_Given_Instruction(directionMap, orientationInstruction));
        }
            return gridLocation;
    }

    public String find_NewOrientation_Given_Instruction(Map<String,String> directionMapper, String instruction){
        return directionMapper.get(instruction);
    }

    public Map<String,String> find_Correct_Direction_Map(GridLocation someLocation) {
        return orientationMap.get(someLocation.getCurrent_CardinalOrientation());
    }

}
