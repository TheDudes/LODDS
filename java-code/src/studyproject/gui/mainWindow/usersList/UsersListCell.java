package studyproject.gui.mainWindow.usersList;

import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListCell;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.GridPane;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.App;
import studyproject.gui.mainWindow.topMenu.TopMenuPresenter;

public class UsersListCell extends ListCell<UserInfo> {

	private Button sendFileButton;
	private Label name;
	private GridPane pane;
	private ImageView userIV = new ImageView();
	TopMenuPresenter topMenuPresenter;
	private ImageView sendFileIV = new ImageView();
	private Image userImage;
	private Image sendFileImage;

	public UsersListCell(TopMenuPresenter topMenuPresenter) {
		super();
		this.topMenuPresenter = topMenuPresenter;

		if (Boolean.valueOf(App.properties.getProperty("icons"))) {
			// load images
			userImage = new Image(getClass().getResourceAsStream("/studyproject/resources/user.png"));
			sendFileImage = new Image(getClass().getResourceAsStream("/studyproject/resources/send-file.png"));

			// image view for user icon
			userIV.setFitWidth(20);
			userIV.setPreserveRatio(true);
			userIV.setFitHeight(20);
			userIV.setImage(userImage);

			sendFileButton = new Button();
			sendFileButton.setPadding(new Insets(3.0));
			sendFileIV.setPreserveRatio(true);
			sendFileIV.setFitWidth(13);
			sendFileIV.setFitHeight(13);
			sendFileIV.setImage(sendFileImage);
			sendFileButton.setText(null);
			sendFileButton.maxHeight(10.0);
			sendFileButton.maxWidth(10.0);
			sendFileButton.setGraphic(sendFileIV);
			name = new Label();
			pane = new GridPane();
			pane.add(userIV, 0, 0);
			pane.add(name, 1, 0);
			pane.setHgap(5.0);
			pane.add(sendFileButton, 2, 0);
			setText(null);
		}
	}

	@Override
	public void updateItem(UserInfo user, boolean empty) {
		super.updateItem(user, empty);
		if (user == null || empty) {
			setText(null);
			setGraphic(null);
		} else {
			if (Boolean.valueOf(App.properties.getProperty("icons"))) {
				sendFileButton.setOnAction((e) -> {
					topMenuPresenter.sendFileToUser(user);
				});
				name.setText(user.toString());
				setGraphic(pane);
			}
			setText(user.toString());
		}
	}
}
