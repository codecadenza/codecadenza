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

import java.util.ArrayList;
import java.util.List;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.scene.control.Button;
import javafx.scene.control.MenuItem;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.input.KeyCodeCombination;

/**
 * <p>
 * Base class for GUI actions
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class Action implements EventHandler<ActionEvent> {
	protected String title;
	protected Image image;
	private final List<MenuItem> menuItems = new ArrayList<>();
	private final List<Button> buttons = new ArrayList<>();

	/**
	 * Create a new menu item and bind it to this action
	 * @return the created menu item
	 */
	public MenuItem createMenuItem() {
		return createMenuItem(null);
	}

	/**
	 * @param keyCodeCombination
	 * @return the created menu item
	 */
	public MenuItem createMenuItem(KeyCodeCombination keyCodeCombination) {
		if (!isEnabled())
			return null;

		final var item = new MenuItem();
		item.setText(title);
		item.setOnAction(this);
		item.setGraphic(new ImageView(image));
		item.setAccelerator(keyCodeCombination);

		menuItems.add(item);

		return item;
	}

	/**
	 * Create a new a button and bind it to this action
	 * @return the button
	 */
	public Button createButton() {
		if (!isEnabled())
			return null;

		final var button = new Button(title);
		button.setOnAction(this);

		buttons.add(button);

		return button;
	}

	/**
	 * Create a new a toolbar button and bind it to this action
	 * @return the created toolbar button
	 */
	public Button createToolbarButton() {
		if (!isEnabled())
			return null;

		final var button = new Button("", new ImageView(image));
		button.setTooltip(new Tooltip(title));
		button.setOnAction(this);

		buttons.add(button);

		return button;
	}

	/**
	 * @param enable
	 */
	public void setEnabled(boolean enable) {
		buttons.forEach(b -> b.setDisable(!enable));

		menuItems.forEach(m -> m.setDisable(!enable));
	}

	/**
	 * @return the title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * @return true if the action is enabled
	 */
	public boolean isEnabled() {
		return true;
	}

	/**
	 * Handle the event
	 */
	public void handle() {
		// The implementation must be provided by a subclass!
	}

	/*
	 * (non-Javadoc)
	 * @see javafx.event.EventHandler#handle(javafx.event.Event)
	 */
	@Override
	public void handle(ActionEvent event) {
		handle();
	}

}
