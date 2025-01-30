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

import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.CMD_CANCEL;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.CMD_OK;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;

import javafx.geometry.Insets;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Separator;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import javafx.stage.Window;

/**
 * <p>
 * Abstract base class for all dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractBaseDialog extends Stage {
	private static final double DEFAULT_WIDTH = 500.0;
	private static final double DEFAULT_HEIGHT = 500.0;

	protected DialogButtonType returnCode = DialogButtonType.CANCEL;
	private HBox panButtons;

	/**
	 * Constructor
	 * @param owner
	 */
	protected AbstractBaseDialog(Window owner) {
		this(owner, null);
	}

	/**
	 * Constructor
	 * @param owner
	 * @param title
	 */
	protected AbstractBaseDialog(Window owner, String title) {
		setTitle(title);

		initOwner(owner);
	}

	/**
	 * @param width
	 * @param height
	 */
	public void setSize(int width, int height) {
		setWidth(width);
		setHeight(height);
	}

	/**
	 * @return the root node of the dialog area
	 */
	protected abstract Node createDialogArea();

	/**
	 * Create the buttons
	 */
	protected void createButtons() {
		addButton(DialogButtonType.OK, getTranslation(CMD_OK), true, false);
		addButton(DialogButtonType.CANCEL, getTranslation(CMD_CANCEL), false, true);
	}

	/**
	 * Add a button to the button bar
	 * @param type
	 * @param label
	 * @param defaultButton
	 * @param cancelButton
	 * @return the new button
	 */
	protected Button addButton(DialogButtonType type, String label, boolean defaultButton, boolean cancelButton) {
		final var button = new Button(label);
		button.setDefaultButton(defaultButton);
		button.setCancelButton(cancelButton);
		button.setOnAction(e -> buttonPressed(type));

		panButtons.getChildren().add(button);

		return button;
	}

	/**
	 * Add a button to the button bar
	 * @param type
	 * @param label
	 * @return the new button
	 */
	protected Button addButton(DialogButtonType type, String label) {
		return addButton(type, label, false, false);
	}

	/**
	 * Callback method that is called as soon as a button is pressed
	 * @param type
	 */
	protected void buttonPressed(DialogButtonType type) {
		returnCode = type;

		if (type == DialogButtonType.OK)
			onOKPressed();
		else if (type == DialogButtonType.CANCEL)
			onCancelPressed();
	}

	/**
	 * Callback method that is called as soon as the 'OK' button is pressed
	 */
	protected void onOKPressed() {
		close();
	}

	/**
	 * Callback method that is called as soon as the 'Cancel' button is pressed
	 */
	protected void onCancelPressed() {
		close();
	}

	/**
	 * @param returnCode
	 */
	protected void setReturnCode(DialogButtonType returnCode) {
		this.returnCode = returnCode;
	}

	/**
	 * @return the button bar
	 */
	protected Node createButtonBar() {
		panButtons = new HBox(10);
		panButtons.setPadding(new Insets(10));

		final var spacer = new Region();

		panButtons.getChildren().add(spacer);

		HBox.setHgrow(spacer, Priority.ALWAYS);

		// Add the buttons
		createButtons();

		return panButtons;
	}

	/**
	 * @return the dialog's root node
	 */
	protected Parent createContents() {
		final var panRoot = new VBox();

		final var panMainContent = new VBox();
		panMainContent.setPadding(new Insets(10, 10, 10, 10));

		final Node panDialogArea = createDialogArea();

		panRoot.getChildren().add(panMainContent);

		VBox.setVgrow(panMainContent, Priority.ALWAYS);
		VBox.setVgrow(panRoot, Priority.ALWAYS);

		panMainContent.getChildren().add(panDialogArea);

		VBox.setVgrow(panDialogArea, Priority.ALWAYS);

		panRoot.getChildren().add(new Separator());
		panRoot.getChildren().add(createButtonBar());

		setOnCloseRequest(e -> onClose());

		return panRoot;
	}

	/**
	 * Callback method that is called after setting the scene of this stage
	 */
	public void onOpen() {

	}

	/**
	 * Callback method that is called when closing the stage
	 */
	public void onClose() {

	}

	/**
	 * Open the dialog
	 * @param block
	 * @return the button type
	 */
	public DialogButtonType open(boolean block) {
		final Parent content = createContents();

		final var scene = new Scene(content, DEFAULT_WIDTH, DEFAULT_HEIGHT);
		scene.getStylesheets().add(getClass().getResource("/css/application.css").toExternalForm());

		setScene(scene);

		onOpen();

		if (block)
			showAndWait();
		else
			show();

		return returnCode;
	}

	/**
	 * Open the dialog and block as long as the dialog is not closed
	 * @return the button type
	 */
	public DialogButtonType open() {
		return open(true);
	}

}
