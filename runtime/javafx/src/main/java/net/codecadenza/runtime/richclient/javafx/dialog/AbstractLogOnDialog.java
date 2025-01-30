/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.richclient.javafx.dialog;

import static javafx.scene.layout.Region.USE_COMPUTED_SIZE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOG_ON_DIALOG_LBL_HOST;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOG_ON_DIALOG_LBL_PASSWORD;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOG_ON_DIALOG_LBL_USER_NAME;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOG_ON_DIALOG_MSG_CONNECT_TO_HOST;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOG_ON_DIALOG_MSG_INVALID_CREDENTIALS;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOG_ON_DIALOG_MSG_MISSING_USER;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOG_ON_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOG_ON_DIALOG_TITLE_MESSAGE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslationForFieldLabel;

import java.lang.invoke.MethodHandles;
import java.util.Map;
import javafx.geometry.HPos;
import javafx.scene.Node;
import javafx.scene.control.ComboBox;
import javafx.scene.control.Label;
import javafx.scene.control.PasswordField;
import javafx.scene.control.TextField;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.stage.StageStyle;
import javafx.stage.Window;
import net.codecadenza.runtime.richclient.javafx.image.ImageLoader;
import net.codecadenza.runtime.richclient.logon.LogOnManager;
import net.codecadenza.runtime.richclient.persistence.entity.LastLogOn;
import net.codecadenza.runtime.richclient.transport.ServiceLocatorDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Abstract base class for log-on dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractLogOnDialog extends TitleAreaDialog {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final int MAX_WRONG_INPUTS = 3;

	private TextField txtUserName;
	private PasswordField txtPassword;
	private int wrongInputCount;
	private ServiceLocatorDTO serviceLocatorDTO;
	private final Map<String, ServiceLocatorDTO> hostMap;
	private ComboBox<String> cboHost;
	private boolean hideHostSelection;

	/**
	 * Constructor
	 * @param owner
	 * @param hostMap a map that contains all available connection configurations
	 */
	protected AbstractLogOnDialog(Window owner, Map<String, ServiceLocatorDTO> hostMap) {
		super(owner, getTranslation(ABSTRACT_LOG_ON_DIALOG_TITLE));

		this.hostMap = hostMap;

		// If the caller provides a list containing only one element we will hide the respective selection component!
		if (hostMap.size() == 1) {
			hideHostSelection = true;

			setSize(340, 220);
		}
		else
			setSize(340, 250);

		// We assume that the caller provides a map containing one item at least!
		serviceLocatorDTO = hostMap.values().stream().findFirst().orElse(null);

		initStyle(StageStyle.UTILITY);
	}

	/**
	 * @param settings
	 * @throws SecurityException if the login has failed
	 */
	public abstract void logOn(ServiceLocatorDTO settings);

	/**
	 * @return the selected service locator DTO
	 */
	public ServiceLocatorDTO getServiceLocatorDTO() {
		return serviceLocatorDTO;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createDialogArea()
	 */
	@Override
	protected Node createDialogArea() {
		setTitleMessage(getTranslation(ABSTRACT_LOG_ON_DIALOG_TITLE_MESSAGE));
		setTitleImage(ImageLoader.getImage(ImageLoader.IMG_LOGON));

		final var panRoot = new GridPane();
		panRoot.setHgap(5.0);
		panRoot.setVgap(5.0);
		panRoot.getColumnConstraints().add(
				new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, false));
		panRoot.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
		panRoot.add(new Label(getTranslationForFieldLabel(ABSTRACT_LOG_ON_DIALOG_LBL_USER_NAME)), 0, 0);
		panRoot.add(txtUserName = new TextField(), 1, 0);
		panRoot.add(new Label(getTranslationForFieldLabel(ABSTRACT_LOG_ON_DIALOG_LBL_PASSWORD)), 0, 1);
		panRoot.add(txtPassword = new PasswordField(), 1, 1);

		if (!hideHostSelection) {
			panRoot.add(new Label(getTranslationForFieldLabel(ABSTRACT_LOG_ON_DIALOG_LBL_HOST)), 0, 2);
			panRoot.add(cboHost = new ComboBox<>(), 1, 2);

			cboHost.getItems().addAll(hostMap.keySet());
			cboHost.setValue(hostMap.keySet().stream().findFirst().orElse(null));
		}

		final LastLogOn lastLogOn = LogOnManager.getLastLogOn();

		if (lastLogOn != null) {
			txtUserName.setText(lastLogOn.getUserName());

			if (!hideHostSelection && hostMap.containsKey(lastLogOn.getHost()))
				cboHost.setValue(lastLogOn.getHost());
		}

		return panRoot;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#onOKPressed()
	 */
	@Override
	protected void onOKPressed() {
		setReturnCode(DialogButtonType.CANCEL);

		if (txtUserName.getText().isEmpty()) {
			setErrorMessage(getTranslation(ABSTRACT_LOG_ON_DIALOG_MSG_MISSING_USER));
			return;
		}

		if (!hideHostSelection)
			serviceLocatorDTO = hostMap.get(cboHost.getValue());

		serviceLocatorDTO.setUserName(txtUserName.getText());
		serviceLocatorDTO.setPassword(txtPassword.getText());

		try {
			logOn(serviceLocatorDTO);

			setReturnCode(DialogButtonType.OK);

			// Save the input
			LogOnManager.saveLastLogOn(txtUserName.getText(), serviceLocatorDTO.getAlias());

			close();
		}
		catch (final SecurityException e) {
			logger.error("Login failed due to bad credentials!", e);

			wrongInputCount++;
			setErrorMessage(getTranslation(ABSTRACT_LOG_ON_DIALOG_MSG_INVALID_CREDENTIALS));

			if (wrongInputCount == MAX_WRONG_INPUTS)
				close();
		}
		catch (final Exception e) {
			logger.error("Login failed!", e);

			final String title = getTranslation(ABSTRACT_LOG_ON_DIALOG_TITLE_MESSAGE);
			final String message = getTranslation(ABSTRACT_LOG_ON_DIALOG_MSG_CONNECT_TO_HOST);

			setErrorMessage(message);
			DialogUtil.openErrorDialog(null, title, message, e);
		}
	}

}
