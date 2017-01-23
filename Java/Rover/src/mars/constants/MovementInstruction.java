package mars.constants;

/**
 * Created by michaelnyika on 11/10/14.
 *
 * This enum just defines what symbols can be used for
 * physical movement of the Rover. In our case, only 1
 * symbol is defined
 */
public enum MovementInstruction {
    ADVANCE("M");

    private String step;
    private MovementInstruction(String move){
        this.step = move;
    }

    public String VALUE(){
        return this.step;
    }
}
