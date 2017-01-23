package mars.constants;

/**
 * Created by michaelnyika on 11/8/14.
 *
 * This enum just defines what symbols can be used for
 * physical orientation of the Rover. In our case, 2
 * symbols are defined for a turn the left and a turn
 * to the right
 *
 */
public enum OrientationInstruction {
    LEFT("L"),
    RIGHT("R");

    private String thisWay_ThatWay;
    private OrientationInstruction(String turnDirection){
        this.thisWay_ThatWay = turnDirection;
    }

    public String way(){
        return this.thisWay_ThatWay;
    }
}
