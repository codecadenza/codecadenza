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
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_ERROR;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_INFO;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.getImage;

import javafx.geometry.HPos;
import javafx.geometry.Insets;
import javafx.geometry.VPos;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.control.Label;
import javafx.scene.control.Separator;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.RowConstraints;
import javafx.scene.layout.VBox;
import javafx.stage.Window;

/**
 * <p>
 * Title area dialog
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class TitleAreaDialog extends AbstractBaseDialog {
	private Label lblMessage;
	private ImageView imgMessage;
	private String titleMessage;
	private Image titleImage;
	private String infoMessage;
	private String errorMessage;

	/**
	 * Constructor
	 * @param owner
	 * @param title
	 * @param titleMessage
	 * @param titleImage
	 */
	protected TitleAreaDialog(Window owner, String title, String titleMessage, Image titleImage) {
		super(owner, title);

		this.titleMessage = titleMessage;
		this.titleImage = titleImage;
	}

	/**
	 * Constructor
	 * @param owner
	 * @param title
	 */
	protected TitleAreaDialog(Window owner, String title) {
		super(owner, title);
	}

	/**
	 * @param titleImage
	 */
	protected void setTitleImage(Image titleImage) {
		this.titleImage = titleImage;
	}

	/**
	 * @param titleMessage
	 */
	protected void setTitleMessage(String titleMessage) {
		this.titleMessage = titleMessage;
	}

	/**
	 * @return the title area
	 */
	private Node createTitleArea() {
		final var panTitle = new GridPane();
		panTitle.setPadding(new Insets(5, 5, 5, 5));
		panTitle.setPrefSize(USE_COMPUTED_SIZE, 55.0);
		panTitle.getColumnConstraints()
				.add(new ColumnConstraints(10.0, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, false));
		panTitle.getColumnConstraints()
				.add(new ColumnConstraints(60.0, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, false));
		panTitle.setStyle("-fx-background-color: white;");

		final var imgViewTitle = new ImageView();
		imgViewTitle.setImage(titleImage);

		panTitle.add(imgViewTitle, 1, 0);

		GridPane.setHalignment(imgViewTitle, HPos.RIGHT);
		GridPane.setHgrow(imgViewTitle, Priority.NEVER);
		GridPane.setVgrow(imgViewTitle, Priority.NEVER);

		final var panLabels = new GridPane();
		panLabels.getColumnConstraints().add(new ColumnConstraints(18.0, 18.0, 18.0, Priority.SOMETIMES, HPos.LEFT, false));
		panLabels.getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));
		panLabels.getRowConstraints().add(new RowConstraints(20, 20, 20, Priority.ALWAYS, VPos.TOP, false));
		panLabels.getRowConstraints().add(new RowConstraints(30, 30, 30, Priority.SOMETIMES, VPos.CENTER, false));

		lblMessage = new Label();

		final var lblTitle = new Label(titleMessage);
		lblTitle.getStyleClass().add("title_message");

		imgMessage = new ImageView();

		panLabels.add(lblTitle, 0, 0, 2, 1);
		panLabels.add(lblMessage, 1, 1);
		panLabels.add(imgMessage, 0, 1);

		GridPane.setHgrow(lblTitle, Priority.ALWAYS);
		GridPane.setValignment(lblTitle, VPos.TOP);

		GridPane.setHgrow(lblMessage, Priority.ALWAYS);
		GridPane.setValignment(lblMessage, VPos.TOP);

		GridPane.setHgrow(imgMessage, Priority.ALWAYS);
		GridPane.setValignment(imgMessage, VPos.TOP);

		panTitle.add(panLabels, 0, 0);

		GridPane.setHgrow(panLabels, Priority.ALWAYS);
		GridPane.setVgrow(panLabels, Priority.ALWAYS);
		GridPane.setFillWidth(panLabels, true);

		return panTitle;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.dialog.Dialog#createContents()
	 */
	@Override
	protected Parent createContents() {
		final var panRoot = new VBox();

		// Create another panel that adds a space between the dialog area and the root panel
		final var panParent = new VBox();
		panParent.setPadding(new Insets(10, 10, 10, 10));

		final Node content = createDialogArea();

		VBox.setVgrow(panParent, Priority.ALWAYS);
		VBox.setVgrow(panRoot, Priority.ALWAYS);

		panParent.getChildren().add(content);

		VBox.setVgrow(content, Priority.ALWAYS);

		panRoot.getChildren().add(createTitleArea());
		panRoot.getChildren().add(new Separator());
		panRoot.getChildren().add(panParent);
		panRoot.getChildren().add(new Separator());
		panRoot.getChildren().add(createButtonBar());

		if (errorMessage != null)
			setErrorMessage(errorMessage);
		else if (infoMessage != null)
			setInfoMessage(infoMessage);

		return panRoot;
	}

	/**
	 * @param message
	 */
	protected void setInfoMessage(String message) {
		if (lblMessage != null)
			lblMessage.setText(message);
		else
			infoMessage = message;

		if (imgMessage != null)
			imgMessage.setImage(getImage(IMG_INFO));
	}

	/**
	 * @param message
	 */
	protected void setErrorMessage(String message) {
		if (lblMessage != null) {
			lblMessage.setText(message);
			lblMessage.setTooltip(new Tooltip(message));
		}
		else
			errorMessage = message;

		if (imgMessage != null)
			imgMessage.setImage(getImage(IMG_ERROR));
	}

}
