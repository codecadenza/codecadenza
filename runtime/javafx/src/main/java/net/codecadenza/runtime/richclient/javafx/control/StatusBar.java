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

import javafx.geometry.HPos;
import javafx.geometry.VPos;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.layout.ColumnConstraints;
import javafx.scene.layout.GridPane;
import javafx.scene.layout.Priority;
import javafx.scene.layout.RowConstraints;

/**
 * <p>
 * Simple status bar that contains a message and a progress area
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class StatusBar extends GridPane {
	private static final double DEFAULT_HEIGHT = 25.0;

	protected Label lblMessage;
	protected ProgressBar progressBar;

	/**
	 * Constructor
	 */
	public StatusBar() {
		setHgap(5.0);
		getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, true));
		getColumnConstraints()
				.add(new ColumnConstraints(USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.ALWAYS, HPos.RIGHT, false));
		getRowConstraints()
				.add(new RowConstraints(DEFAULT_HEIGHT, DEFAULT_HEIGHT, DEFAULT_HEIGHT, Priority.NEVER, VPos.CENTER, true));

		add(lblMessage = new Label(), 0, 0);
		add(progressBar = new ProgressBar(0), 1, 0);

		GridPane.setFillWidth(lblMessage, true);
	}

	/**
	 * @param text
	 */
	public void setText(String text) {
		lblMessage.setText(text);
	}

	/**
	 * Show an indeterminate progress
	 */
	public void showProgress() {
		progressBar.setProgress(-1);
	}

	/**
	 * Show the progress
	 * @param progress
	 */
	public void showProgress(Double progress) {
		progressBar.setProgress(progress);
	}

	/**
	 * Stop showing a progress
	 */
	public void stopProgress() {
		progressBar.setProgress(0);
	}

	/**
	 * @param visible
	 */
	public void setProgressBarVisible(boolean visible) {
		progressBar.setVisible(visible);
	}

}
