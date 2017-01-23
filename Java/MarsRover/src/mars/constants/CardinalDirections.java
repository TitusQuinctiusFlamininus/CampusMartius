package mars.constants;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * Represent the 4 simple cardinal directions that the Rover is
 * capable of orienting itself to.
 */
public enum CardinalDirections {
    NORTH ("N"), EAST("E"), SOUTH("S"), WEST("W");

    private String direction ;

    private CardinalDirections(String symbol){
        this.direction = symbol;
    }

    public String DIR(){
        return direction;
    }


}
