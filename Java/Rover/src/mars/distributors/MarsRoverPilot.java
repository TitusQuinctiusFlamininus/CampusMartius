package mars.distributors;

import mars.constants.MovementInstruction;
import mars.constants.OrientationInstruction;
import mars.enforcers.MarsMovementEnforcer;
import mars.enforcers.MarsOrientationEnforcer;
import mars.exceptions.DisallowedRoverMovementException;
import mars.interfaces.IMovementEnforcer;
import mars.interfaces.IOrientationEnforcer;
import mars.validators.LandingPositionInputValidator;
import mars.validators.NavigationInputValidator;
import mars.valueobjects.GridLocation;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This is the main executor module of the Rover. It iterates through the
 * commands (once validates) one at a time and gives Tiffi a status report
 * of how the Rover is moving as well as the Rover (and hence Tiffi's) final position
 */
public class MarsRoverPilot {

    private LandingPositionInputValidator landingPositionValidator;
    private NavigationInputValidator navigationValidator;
    private IOrientationEnforcer orienter;
    private IMovementEnforcer mover;

    public MarsRoverPilot(){
        setLandingPositionValidator(new LandingPositionInputValidator());
        setNavigationValidator(new NavigationInputValidator());
        setOrienter(new MarsOrientationEnforcer());
        setMover(new MarsMovementEnforcer());
    }

    public GridLocation distribute_candies(String initialPosition, String navigationInstructions) throws Exception{

        System.out.println("Mars Rover to Tiffi: Instructions from NASA Intercepted.");
        System.out.println("========================================================");
        System.out.println("Your Mars Landing Position is : [ "+initialPosition+" ] ");
        System.out.println("Your Navigation Sequence for Candy Distribution is : [ "+navigationInstructions+" ] \n");


        GridLocation init_Location = null;
        if(getLandingPositionValidator().validateInitialLandingPosition(initialPosition)){
            if(getNavigationValidator().validateNavigationalInput(navigationInstructions)){
                init_Location = new GridLocation();
                int x_initial = Character.getNumericValue(initialPosition.charAt(0));
                int y_initial = Character.getNumericValue(initialPosition.charAt(1));
                String orientation_initial = String.valueOf(initialPosition.charAt(2));

                init_Location.setCurrent_X_position(x_initial);
                init_Location.setCurrent_Y_position(y_initial);
                init_Location.setCurrent_CardinalOrientation(orientation_initial);

                System.out.println("Tiffi here: Beginning Candy distribution...");
                for(char naviCommand : navigationInstructions.toCharArray()){
                    System.out.println("...");
                    init_Location = orient_rover(init_Location, naviCommand);
                    init_Location = move_rover(init_Location, naviCommand);
                }

                System.out.println("========================================================");
                System.out.println("Tiffi here: Candy distribution Complete, Houston. Over and Out!");

            }
        }
        report_and_sign_out(init_Location);
        return init_Location;

    }

    private void report_and_sign_out(GridLocation init_Location) {
        System.out.println("No more Instructions from NASA....");
        System.out.println("The final Position and Orientation of the Rover is: "+
        init_Location.getCurrent_X_position()+
                init_Location.getCurrent_Y_position()+
                init_Location.getCurrent_CardinalOrientation());
    }

    private GridLocation move_rover(GridLocation init_Location, char naviCommand) throws DisallowedRoverMovementException {
        if(String.valueOf(naviCommand).equals(MovementInstruction.ADVANCE.VALUE())) {
            init_Location = getMover().moveRover(init_Location);
            System.out.println("Rover-Moved-to-New-GridPosition : "+
                    init_Location.getCurrent_X_position()+
                    init_Location.getCurrent_Y_position()+
                    init_Location.getCurrent_CardinalOrientation()+"\n");
        }
        return init_Location;
    }

    private GridLocation orient_rover(GridLocation init_Location, char naviCommand) {
        if(String.valueOf(naviCommand).equals(OrientationInstruction.LEFT.way()) ||
                String.valueOf(naviCommand).equals(OrientationInstruction.RIGHT.way())) {
            init_Location = getOrienter().orientRover(init_Location, String.valueOf(naviCommand));
            System.out.println("Rover-Oriented-to-New-Direction : " +
                    init_Location.getCurrent_X_position() +
                    init_Location.getCurrent_Y_position() +
                    init_Location.getCurrent_CardinalOrientation() + "\n");
        }
        return init_Location;
    }

    public LandingPositionInputValidator getLandingPositionValidator() {
        return landingPositionValidator;
    }

    public void setLandingPositionValidator(LandingPositionInputValidator landingPositionValidator) {
        this.landingPositionValidator = landingPositionValidator;
    }

    public NavigationInputValidator getNavigationValidator() {
        return navigationValidator;
    }

    public void setNavigationValidator(NavigationInputValidator navigationValidator) {
        this.navigationValidator = navigationValidator;
    }

    public IOrientationEnforcer getOrienter() {
        return orienter;
    }

    public void setOrienter(IOrientationEnforcer orienter) {
        this.orienter = orienter;
    }

    public IMovementEnforcer getMover() {
        return mover;
    }

    public void setMover(IMovementEnforcer mover) {
        this.mover = mover;
    }
}
