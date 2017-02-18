package studyproject.gui.sendPermissionDialog;

import javafx.stage.Stage;
import studyproject.API.Core.Request.GetPermissionRequest;

/**
 * JavaFX Stage Wrapper. This Wrapper is used like a normal Stage with an
 * additional GetPermissionRequest variable
 * @author Chris
 *
 */
public class SendPermissionDialog extends Stage {
	private GetPermissionRequest getPermissionRequest = null;

	public SendPermissionDialog(GetPermissionRequest getPermissionRequest) {
		this.getPermissionRequest = getPermissionRequest;
	}
	
	public GetPermissionRequest getPermissionRequest() {
		return getPermissionRequest;
	}

	public void setPermissionRequest(GetPermissionRequest getPermissionRequest) {
		this.getPermissionRequest = getPermissionRequest;
	}
}
