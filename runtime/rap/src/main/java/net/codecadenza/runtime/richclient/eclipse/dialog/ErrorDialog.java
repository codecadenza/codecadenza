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
package net.codecadenza.runtime.richclient.eclipse.dialog;

import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Dialog for displaying exception details
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ErrorDialog extends AbstractErrorDialog {
	private static final long serialVersionUID = 8635816891385513610L;

	/**
	 * Constructor
	 * @param parentShell
	 * @param title
	 * @param message
	 * @param t
	 */
	private ErrorDialog(final Shell parentShell, final String title, final String message, final Throwable t) {
		super(parentShell, title, message, t);
	}

	/**
	 * Open the error dialog
	 * @param parentShell
	 * @param title the window title
	 * @param message the error message
	 * @param throwable an error to be shown in details section
	 */
	public static void showError(final Shell parentShell, final String title, final String message, final Throwable throwable) {
		new ErrorDialog(parentShell, title, message, throwable);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
	 */
	@Override
	protected Point getInitialSize() {
		return DialogUtility.adaptSizeToSystemDPI(500, 400);
	}

}
