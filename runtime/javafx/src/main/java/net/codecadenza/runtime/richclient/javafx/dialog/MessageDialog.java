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
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.CMD_NO;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.CMD_OK;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.CMD_YES;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.MESSAGE_DIALOG_LBL_DETAILS;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslationForFieldLabel;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_DIALOG_CONFIRM;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_DIALOG_ERROR;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_DIALOG_INFO;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_DIALOG_WARNING;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.getImage;

import java.io.PrintWriter;
import java.io.StringWriter;
import javafx.geometry.HPos;
import javafx.scene.Node;
import javafx.scene.control.Label;
import javafx.scene.control.TextArea;
import javafx.scene.image.ImageView;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.stage.Modality;
import javafx.stage.StageStyle;
import javafx.stage.Window;

/**
 * <p>
 * Dialog for displaying different kinds of messages
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class MessageDialog extends AbstractBaseDialog {
	private final String message;
	private final MessageDialogType type;
	private Throwable error;

	/**
	 * Constructor
	 * @param owner
	 * @param dialogType
	 * @param title
	 * @param message
	 */
	public MessageDialog(Window owner, MessageDialogType dialogType, String title, String message) {
		super(owner, title);

		this.message = message;
		this.type = dialogType;

		setSize(400, 150);

		initModality(Modality.APPLICATION_MODAL);
		initStyle(StageStyle.UTILITY);
	}

	/**
	 * Constructor
	 * @param owner
	 * @param dialogType
	 * @param title
	 * @param message
	 * @param error
	 */
	public MessageDialog(Window owner, MessageDialogType dialogType, String title, String message, Throwable error) {
		this(owner, dialogType, title, message);

		this.error = error;

		if (error != null)
			setSize(550, 500);
	}

	/**
	 * Constructor
	 * @param owner
	 * @param dialogType
	 * @param title
	 * @param error
	 */
	public MessageDialog(Window owner, MessageDialogType dialogType, String title, Throwable error) {
		this(owner, dialogType, title, null, error);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createDialogArea()
	 */
	@Override
	protected Node createDialogArea() {
		final var panContent = new GridPane();
		panContent.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
		panContent.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.NEVER, HPos.RIGHT, false));
		panContent.setHgap(5);
		panContent.setVgap(5);

		final var lblMsg = new Label();

		if (message != null)
			lblMsg.setText(message);

		if (message == null && error != null)
			lblMsg.setText(error.getMessage());

		lblMsg.setWrapText(true);

		final var imageView = new ImageView();

		if (type == MessageDialogType.INFORMATION)
			imageView.setImage(getImage(IMG_DIALOG_INFO));
		else if (type == MessageDialogType.WARNING)
			imageView.setImage(getImage(IMG_DIALOG_WARNING));
		else if (type == MessageDialogType.CONFIRMATION)
			imageView.setImage(getImage(IMG_DIALOG_CONFIRM));
		else if (type == MessageDialogType.ERROR)
			imageView.setImage(getImage(IMG_DIALOG_ERROR));

		panContent.add(lblMsg, 0, 0);
		panContent.add(imageView, 1, 0);

		if (error != null) {
			final var sw = new StringWriter();
			final var pw = new PrintWriter(sw);

			error.printStackTrace(pw);

			final var lblDetails = new Label(getTranslationForFieldLabel(MESSAGE_DIALOG_LBL_DETAILS));

			final var txtDetails = new TextArea(sw.toString());
			txtDetails.setEditable(false);
			txtDetails.setWrapText(false);

			panContent.add(lblDetails, 0, 1, 2, 1);
			panContent.add(txtDetails, 0, 2, 2, 1);

			GridPane.setVgrow(txtDetails, Priority.ALWAYS);
		}

		return panContent;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#buttonPressed(net.codecadenza.runtime.richclient.
	 * javafx.dialog.DialogButtonType)
	 */
	@Override
	protected void buttonPressed(DialogButtonType type) {
		super.buttonPressed(type);

		close();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createButtons()
	 */
	@Override
	protected void createButtons() {
		if (type == MessageDialogType.CONFIRMATION) {
			addButton(DialogButtonType.YES, getTranslation(CMD_YES), true, false);
			addButton(DialogButtonType.NO, getTranslation(CMD_NO), false, true);
		}
		else
			addButton(DialogButtonType.OK, getTranslation(CMD_OK), true, false);
	}

}
