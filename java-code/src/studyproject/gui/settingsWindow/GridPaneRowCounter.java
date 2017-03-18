package studyproject.gui.settingsWindow;

/**
 * Helper for JavaFX to automatically insert the current row number in fxml files
 * @author gitmalong
 *
 */
public class GridPaneRowCounter {
	
    private int currentRow = 0;

    public int getCurrentRow() {
        return currentRow;
    }

    public int getCurrentRowAndIncrement() {
        return currentRow++;
    }
}