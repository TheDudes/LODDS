package studyproject.gui.introduction;

import java.net.URL;
import java.util.ResourceBundle;

import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.CheckBox;
import javafx.scene.control.Label;

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

	@Override
	public void initialize(URL location, ResourceBundle resources) {
		// TODO Auto-generated method stub

	}

}
