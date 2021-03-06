package studyproject.gui.mainWindow.logArea;

import java.net.URL;
import java.util.ResourceBundle;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.inject.Inject;

import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.CheckMenuItem;
import javafx.scene.control.ScrollBar;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.cell.PropertyValueFactory;
import studyproject.App;
import studyproject.API.Errors.Error;

public class LogAreaPresenter implements Initializable {

    public static int NR_OF_ENTRANCES_TO_START_AUTOSCROLL = 10;

    @FXML
    CheckMenuItem errorCB;
    @FXML
    CheckMenuItem infoCB;
    @FXML
    CheckMenuItem getRecCB;
    @FXML
    CheckMenuItem getSentCB;
    @FXML
    CheckMenuItem broadcastCB;
    @FXML
    TableView<Error> logTableView;
    @FXML
    TableColumn<Error, String> logKeyCol;
    @FXML
    TableColumn<Error, String> sourceMethodCol;
    @FXML
    TableColumn<Error, String> sourceClassCol;
    @FXML
    TableColumn<Error, String> msgCol;
    @FXML
    TableColumn<Error, String> timestampCol;
    @FXML
    TableColumn<Error, String> logLvlCol;

    @Inject
    LogAreaModel logAreaModel;

    @Override
    public void initialize(URL location, ResourceBundle resources) {
        setLogAreaCheckBoxesSelectedProperties();

        ObservableList<Error> errors = FXCollections.observableArrayList();
        logTableView.setItems(errors);

        addAutoScroll(logTableView);

        setTableColumnCellFactories();

        bindSelectedPropertiesToModel();

        Logger.getGlobal().addHandler(new LogAreaHandler(Level.ALL, logTableView, logAreaModel));

    }

    /**
     * Sets the selected properties of all Checkboxes(errorCB, infoCB, getRecCB,
     * getSentCB, broadcastCB) to the values parsed from App.properties
     */
    private void setLogAreaCheckBoxesSelectedProperties() {
        Platform.runLater(new Runnable() {

            @Override
            public void run() {
                errorCB.setSelected(Boolean.valueOf(App.properties.getProperty("errorCB")));
                infoCB.setSelected(Boolean.valueOf(App.properties.getProperty("infoCB")));
                getRecCB.setSelected(Boolean.valueOf(App.properties.getProperty("getRecCB")));
                getSentCB.setSelected(Boolean.valueOf(App.properties.getProperty("getSentCB")));
                broadcastCB.setSelected(Boolean.valueOf(App.properties.getProperty("broadcastCB")));

            }
        });
    }

    /**
     * Binds the selected properties of all Checkboxes(errorCB, infoCB,
     * getRecCB, getSentCB, broadcastCB) to the corresponding
     * SimpleBooleanProperties of the logAreaModel
     */
    private void bindSelectedPropertiesToModel() {
        errorCB.selectedProperty().bindBidirectional(logAreaModel.getError());
        infoCB.selectedProperty().bindBidirectional(logAreaModel.getInfo());
        getRecCB.selectedProperty().bindBidirectional(logAreaModel.getGetRec());
        getSentCB.selectedProperty().bindBidirectional(logAreaModel.getGetSent());
        broadcastCB.selectedProperty().bindBidirectional(logAreaModel.getBroadcast());
    }

    /**
     * Sets all table column cell value factories
     */
    private void setTableColumnCellFactories() {
        timestampCol.setCellValueFactory(new PropertyValueFactory<Error, String>("timestamp"));
        logKeyCol.setCellValueFactory(new PropertyValueFactory<Error, String>("logKey"));
        sourceMethodCol.setCellValueFactory(new PropertyValueFactory<Error, String>("sourceMethodName"));
        sourceClassCol.setCellValueFactory(new PropertyValueFactory<Error, String>("sourceClassName"));
        msgCol.setCellValueFactory(new PropertyValueFactory<Error, String>("msg"));
        logLvlCol.setCellValueFactory(new PropertyValueFactory<Error, String>("logLevelString"));
    }

    /**
     * Adds an auto scroll property to the in the parameter list passed
     * TableView, which causes the TableView to automatically scroll down if an
     * entry is made and the scroll bar is at the bottom of the view
     *
     * @param view The TableView where the auto scroll functionality shall be
     *             added
     */
    public static <S> void addAutoScroll(final TableView<S> view) {
        if (view == null) {
            throw new NullPointerException();
        }

        view.getItems().addListener((ListChangeListener<S>) (c -> {
            c.next();
            final ScrollBar scrollBar = (ScrollBar) view.lookup(".scroll-bar:vertical");
            final int size = view.getItems().size();
            if ((size > 0 && (scrollBar.valueProperty().get() >= scrollBar.getMax()))
                    || (size > 0) && (size < NR_OF_ENTRANCES_TO_START_AUTOSCROLL)) {
                Platform.runLater(new Runnable() {
                    @Override
                    public void run() {
                        view.scrollTo(size - 1);
                    }
                });
            }
        }));
    }

}
