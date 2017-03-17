package studyproject.gui.mainWindow.userList;

import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.TableCell;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import studyproject.API.Lvl.Mid.Core.UserInfo;
import studyproject.gui.mainWindow.topMenu.TopMenuPresenter;

public class SendFileButtonCell extends TableCell<UserInfo, Boolean> {
	final Button cellButton = new Button("send-file");

	SendFileButtonCell(Image sendFileImage, TopMenuPresenter topMenuPresenter) {
		cellButton.setMaxHeight(22.0);
		cellButton.setMaxWidth(22.0);
		cellButton.setPadding(new Insets(0, 0, 0, 0));
		if (sendFileImage != null) {
			cellButton.setGraphic(new ImageView(sendFileImage));
			cellButton.setText(null);
			cellButton.setStyle("-fx-background-color: null;");
		}
		cellButton.setOnAction((e) -> {
			topMenuPresenter.sendFileToUser(((UserInfo) getTableRow().getItem()));
		});

	}

	// Display button if the row is not empty
	@Override
	protected void updateItem(Boolean userInfo, boolean empty) {
		super.updateItem(userInfo, empty);
		if (!empty && userInfo != null) {
			setGraphic(cellButton);
		}
	}
}