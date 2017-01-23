package mars.builders;

import mars.constants.CardinalDirections;
import mars.constants.OrientationInstruction;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by michaelnyika on 11/9/14.
 *
 * The purpose of this class is to build the rules of how the
 * rover ought to orient itself and also have available the
 * master map to the MapOrientationEnforcer object
 */
public class OrientationMapBuilder {

    private HashMap<String, String> N_instructionToDirection;
    private HashMap<String, String> S_instructionToDirection;
    private HashMap<String, String> E_instructionToDirection;
    private HashMap<String, String> W_instructionToDirection;
    private HashMap<String, Map<String,String>> orientationMap;

    public OrientationMapBuilder(){
        compose_North_Orientation_Rules();
        compose_East_Orientation_Rules();
        compose_South_Orientation_Rules();
        compose_West_Orientation_Rules();
        construct_Master_Orientation_Mapping();
    }

    public void construct_Master_Orientation_Mapping() {
        setOrientationMap(new HashMap<String, Map<String,String>>());
        getOrientationMap().put(CardinalDirections.NORTH.DIR(), N_instructionToDirection);
        getOrientationMap().put(CardinalDirections.SOUTH.DIR(), S_instructionToDirection);
        getOrientationMap().put(CardinalDirections.EAST.DIR(), E_instructionToDirection);
        getOrientationMap().put(CardinalDirections.WEST.DIR(), W_instructionToDirection);
    }

    public void compose_North_Orientation_Rules() {
        N_instructionToDirection = new HashMap<String, String>();
        N_instructionToDirection.put("NAME", "NORTH_FACING");
        N_instructionToDirection.put(OrientationInstruction.LEFT.way(), CardinalDirections.WEST.DIR());
        N_instructionToDirection.put(OrientationInstruction.RIGHT.way(), CardinalDirections.EAST.DIR());
    }

    public void compose_South_Orientation_Rules() {
        S_instructionToDirection = new HashMap<String, String>();
        S_instructionToDirection.put("NAME", "SOUTH_FACING");
        S_instructionToDirection.put(OrientationInstruction.LEFT.way(), CardinalDirections.EAST.DIR());
        S_instructionToDirection.put(OrientationInstruction.RIGHT.way(), CardinalDirections.WEST.DIR());
    }

    public void compose_East_Orientation_Rules() {
        E_instructionToDirection = new HashMap<String, String>();
        E_instructionToDirection.put("NAME", "EAST_FACING");
        E_instructionToDirection.put(OrientationInstruction.LEFT.way(), CardinalDirections.NORTH.DIR());
        E_instructionToDirection.put(OrientationInstruction.RIGHT.way(), CardinalDirections.SOUTH.DIR());
    }

    public void compose_West_Orientation_Rules() {
        W_instructionToDirection = new HashMap<String, String>();
        W_instructionToDirection.put("NAME", "WEST_FACING");
        W_instructionToDirection.put(OrientationInstruction.LEFT.way(), CardinalDirections.SOUTH.DIR());
        W_instructionToDirection.put(OrientationInstruction.RIGHT.way(), CardinalDirections.NORTH.DIR());
    }

    public HashMap<String, Map<String, String>> getOrientationMap() {
        return orientationMap;
    }

    public void setOrientationMap(HashMap<String, Map<String, String>> orientationMap) {
        this.orientationMap = orientationMap;
    }
}
