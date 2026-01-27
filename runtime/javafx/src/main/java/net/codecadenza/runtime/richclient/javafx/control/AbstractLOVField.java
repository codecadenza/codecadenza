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
package net.codecadenza.runtime.richclient.javafx.control;

import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ABSTRACT_LOV_FIELD_CMD_OPEN;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;

import javafx.geometry.HPos;
import javafx.scene.control.Button;
import javafx.scene.control.TextField;
import javafx.scene.control.Tooltip;
import javafx.scene.image.ImageView;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import net.codecadenza.runtime.richclient.javafx.dialog.AbstractLOVDialog;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogButtonType;
import net.codecadenza.runtime.richclient.javafx.image.ImageLoader;

/**
 * <p>
 * Abstract list-of-values field
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the LOV field
 * @param <X> the type of the LOV dialog
 */
public abstract class AbstractLOVField<T, X> extends GridPane {
	private final AbstractLOVDialog<X> lovDialog;
	private T selectedItem;
	private TextField txtValue;
	private Button cmdOpen;
	private boolean editable = true;

	/**
	 * Constructor
	 * @param lovDialog
	 */
	protected AbstractLOVField(AbstractLOVDialog<X> lovDialog) {
		this.lovDialog = lovDialog;

		initialize();
	}

	/**
	 * @param selectedItem
	 * @return the string to be displayed
	 */
	public abstract String getItemText(T selectedItem);

	/**
	 * @param selectedItem
	 * @return the converted item
	 */
	@SuppressWarnings("unused")
	public T convertSelection(X selectedItem) {
		return null;
	}

	/**
	 * @return the selected item
	 */
	public T getSelectedItem() {
		return selectedItem;
	}

	/**
	 * @param selectedItem
	 */
	public void setSelectedItem(T selectedItem) {
		this.selectedItem = selectedItem;

		if (selectedItem != null)
			txtValue.setText(getItemText(selectedItem));
		else
			txtValue.setText("");
	}

	/**
	 * @param editable
	 */
	public void setEditable(boolean editable) {
		this.editable = editable;

		cmdOpen.setDisable(!editable);
	}

	/**
	 * Open the corresponding list-of-values dialog
	 */
	private void openLOV() {
		if (!editable)
			return;

		final DialogButtonType returnCode = lovDialog.open();

		if (returnCode == DialogButtonType.OK)
			setSelectedItem(convertSelection(lovDialog.getSelection()));
		else if (returnCode == DialogButtonType.RESET)
			setSelectedItem(null);
	}

	/**
	 * Initialize the component
	 */
	private void initialize() {
		setHgap(5.0);
		getColumnConstraints().add(
				new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, false));
		getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.LEFT, true));

		txtValue = new TextField();
		txtValue.setEditable(false);
		txtValue.setOnMouseClicked(_ -> openLOV());

		cmdOpen = new Button();
		cmdOpen.setGraphic(new ImageView(ImageLoader.getImage(ImageLoader.IMG_LOV)));
		cmdOpen.setOnAction(_ -> openLOV());
		cmdOpen.setTooltip(new Tooltip(getTranslation(ABSTRACT_LOV_FIELD_CMD_OPEN)));

		add(txtValue, 0, 0);
		add(cmdOpen, 1, 0);
	}

	/**
	 * @param tooltip
	 */
	public void setTooltip(Tooltip tooltip) {
		txtValue.setTooltip(tooltip);
	}

}
