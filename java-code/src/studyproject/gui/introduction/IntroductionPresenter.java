package studyproject.gui.introduction;

import java.net.URL;
import java.util.ResourceBundle;

import javax.inject.Inject;

import javafx.application.Platform;
import javafx.beans.binding.Bindings;
import javafx.beans.binding.BooleanBinding;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;
import studyproject.App;
import studyproject.gui.mainWindow.MainWindowModel;
import studyproject.gui.mainWindow.filesTree.FilesTreeModel;
import studyproject.gui.mainWindow.logArea.LogAreaModel;
import studyproject.gui.mainWindow.tasksList.TasksListModel;
import studyproject.gui.mainWindow.userList.UserListModel;

public class IntroductionPresenter implements Initializable {

	@FXML
	private Button cancelBtn;
	@FXML
	private Button nextBtn;
	@FXML
	private Button prevBtn;
	@FXML
	private CheckBox dontShowAgainCB;
	@FXML
	private Label textLabel;
	@FXML
	private Label title;

	@Inject
	IntroductionModel introductionModel;
	@Inject
	MainWindowModel mainWindowModel;
	@Inject
	FilesTreeModel filesTreeModel;
	@Inject
	UserListModel userListmodel;
	@Inject
	TasksListModel taskListModel;
	@Inject
	LogAreaModel logAreaModel;

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		add(mainWindowModel.getViewTitle(), mainWindowModel.getViewDiscription());
		add(filesTreeModel.getViewTitle(), filesTreeModel.getViewDiscription());
		add(userListmodel.getViewTitle(), userListmodel.getViewDiscription());
		add(taskListModel.getViewTitle(), taskListModel.getViewDiscription());
		add(logAreaModel.getViewTitle(), logAreaModel.getViewDiscription());
		//!!This should always be the last add
		add("Finished", "You finished the Introduction");

		// disable prev button on index one
		BooleanBinding disablePrevAtFirst = Bindings.equal(0, introductionModel.getIndex());
		// invalidate to recalculate value
		disablePrevAtFirst.invalidate();
		prevBtn.disableProperty().bind(disablePrevAtFirst);
		BooleanBinding disableNextAtLast = Bindings.equal(introductionModel.getTitles().size() - 1,
				introductionModel.getIndex());
		disableNextAtLast.addListener((obs, oldVal, newVal) -> {
			if (newVal == true) {
				Platform.runLater(() -> {
					nextBtn.textProperty().setValue("Finish!");
				});
				nextBtn.setOnAction((e) -> closeDialog());
			}
			if (newVal == false) {
				Platform.runLater(() -> {
					nextBtn.setText("Next");
				});
				nextBtn.setOnAction((e) -> nextView());
			}
		});

		// cancel button closes dialog
		cancelBtn.setOnAction((e) -> closeDialog());

		nextBtn.setOnAction((e) -> {
			nextView();
			// needs to be done so the value is refreshed
			disableNextAtLast.invalidate();
		});

		prevBtn.setOnAction((e) -> {
			prevView();
			// needs to be done so the value is refreshed
			disablePrevAtFirst.invalidate();
		});

		// to init the first view (MainWindow)
		setViewFromIndex(0, 0);
	}

	public void closeDialog() {
		App.properties.setProperty("introduction", String.valueOf(dontShowAgainCB.selectedProperty().not().getValue()));
		prevBtn.getParent().getScene().getWindow().hide();
	}

	private void nextView() {
		Platform.runLater(() -> {
			setViewFromIndex(1, 1);
		});
	}

	private void prevView() {
		Platform.runLater(() -> {
			setViewFromIndex(-1, -1);
		});
	}

	private void setViewFromIndex(int indexModifier, int afterwork) {
		title.setText(introductionModel.getTitles().get(introductionModel.getIndex().intValue() + indexModifier));
		textLabel.setText(
				introductionModel.getDiscriptions().get(introductionModel.getIndex().intValue() + indexModifier));
		introductionModel.getIndex().setValue(introductionModel.getIndex().getValue() + afterwork);
	}

	private void add(String title, String discription) {
		introductionModel.getTitles().add(title);
		introductionModel.getDiscriptions().add(discription);
	}

}
