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

import javafx.stage.Window;

/**
 * <p>
 * Utility class for opening message dialogs in a convenient way
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DialogUtil {
	/**
	 * Constructor
	 */
	private DialogUtil() {

	}

	/**
	 * Open an information dialog
	 * @param owner
	 * @param title
	 * @param message
	 */
	public static void openInformationDialog(Window owner, String title, String message) {
		new MessageDialog(owner, MessageDialogType.INFORMATION, title, message).open();
	}

	/**
	 * Open a confirmation dialog
	 * @param owner
	 * @param title
	 * @param message
	 * @return the type of the button that has been pressed
	 */
	public static DialogButtonType openConfirmationDialog(Window owner, String title, String message) {
		return new MessageDialog(owner, MessageDialogType.CONFIRMATION, title, message).open();
	}

	/**
	 * Open a warning dialog
	 * @param owner
	 * @param title
	 * @param message
	 */
	public static void openWarningDialog(Window owner, String title, String message) {
		new MessageDialog(owner, MessageDialogType.WARNING, title, message).open();
	}

	/**
	 * Open an error dialog
	 * @param owner
	 * @param title
	 * @param message
	 * @param error
	 */
	public static void openErrorDialog(Window owner, String title, String message, Throwable error) {
		new MessageDialog(owner, MessageDialogType.ERROR, title, message, error).open();
	}

	/**
	 * Open an error dialog
	 * @param owner
	 * @param title
	 * @param error
	 */
	public static void openErrorDialog(Window owner, String title, Throwable error) {
		new MessageDialog(owner, MessageDialogType.ERROR, title, error).open();
	}

}
