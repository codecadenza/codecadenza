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

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_OK;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ERROR_DIALOG_HIDE_DETAILS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ERROR_DIALOG_SHOW_DETAILS;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.UndeclaredThrowableException;
import java.sql.SQLException;
import java.util.Date;
import java.util.TreeMap;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Abstract base class for error dialogs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractErrorDialog extends Dialog {
	private enum DetailState {
		DETAILS_HIDE, DETAILS_SHOW
	}

	private Text txtDetails;
	private DetailState state = DetailState.DETAILS_HIDE;
	private final String title;
	private final String message;
	private final Throwable t;

	/**
	 * Constructor
	 * @param parentShell
	 * @param title
	 * @param message
	 * @param t
	 */
	protected AbstractErrorDialog(final Shell parentShell, final String title, final String message, final Throwable t) {
		super(parentShell);

		this.title = title;
		this.message = message;
		this.t = t;
		this.setShellStyle(SWT.CLOSE | SWT.RESIZE);

		open();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);
		panDialogArea.setLayout(new GridLayout(2, false));

		final var lblIcon = new Label(panDialogArea, SWT.NONE);
		lblIcon.setImage(ImageCache.getImage(ImageCache.IMG_ERROR));

		final var lblMessage = new Label(panDialogArea, SWT.NONE);
		lblMessage.setText(message);

		txtDetails = new Text(panDialogArea, SWT.V_SCROLL | SWT.MULTI | SWT.BORDER | SWT.WRAP | SWT.H_SCROLL);
		txtDetails.setBackground(new Color(null, 255, 255, 255));
		txtDetails.setEditable(false);
		txtDetails.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1));
		txtDetails.setText(getErrorDetails(t));

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.OK_ID, getTranslation(CMD_OK), true);
	}

	/**
	 * Get the error details
	 * @param t the exception that was thrown
	 * @return a string with detail information
	 */
	static String getErrorDetails(Throwable t) {
		final var sb = new StringBuilder();
		final String nl = System.lineSeparator();

		if (t instanceof final UndeclaredThrowableException undeclaredThrowableException)
			t = undeclaredThrowableException.getUndeclaredThrowable();

		sb.append(new Date()).append(nl);

		if (t != null) {
			// Print the exception details
			sb.append(nl).append("-----------------------------------------------").append(nl);
			sb.append("Exception details.").append(nl).append(nl);
			sb.append("Class: ").append(t.getClass().getName()).append(nl);
			sb.append("Message: ").append(t.getMessage()).append(nl);

			printError(t, "Stack trace:", sb);
		}

		// Print the system properties
		sb.append(nl).append("-----------------------------------------------").append(nl);
		sb.append("System properties:").append(nl).append(nl);

		new TreeMap<>(System.getProperties()).entrySet()
				.forEach(entry -> sb.append(entry.getKey()).append("=").append(entry.getValue()).append(nl));

		// Print runtime information
		sb.append(nl).append("-----------------------------------------------").append(nl);
		sb.append("Runtime info:").append(nl).append(nl);

		final Runtime rt = Runtime.getRuntime();

		sb.append("Memory TOTAL / FREE / MAX: ").append(rt.totalMemory()).append(" / ");
		sb.append(rt.freeMemory()).append(" / ").append(rt.maxMemory()).append(nl);
		sb.append("Available processors: ").append(rt.availableProcessors()).append(nl);
		sb.append("System class loader: ").append("" + ClassLoader.getSystemClassLoader()).append(nl);
		sb.append("Thread context class loader: ").append("" + Thread.currentThread().getContextClassLoader()).append(nl);

		return sb.toString();
	}

	/**
	 * Print the error method
	 * @param t the exception
	 * @param header the header to print
	 * @param sb a {@link StringBuilder} that contains detail information
	 */
	private static void printError(final Throwable t, final String header, final StringBuilder sb) {
		if (t == null)
			return;

		final String nl = System.lineSeparator();
		sb.append(nl).append(header).append(nl).append(nl);

		for (final StackTraceElement stackTraceElement : t.getStackTrace())
			sb.append(stackTraceElement.toString()).append(nl);

		Throwable next = t.getCause();

		printError(next, "Caused by " + next, sb);

		if (t instanceof final SQLException sqlException) {
			// Add special handling in case of a SQLException
			next = sqlException.getNextException();

			printError(next, "Next exception: " + next, sb);
		}
		else if (t instanceof final InvocationTargetException invocationTargetException) {
			// Add special handling in case of an InvocationTargetException
			next = invocationTargetException.getTargetException();

			printError(next, "Target exception: " + next, sb);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.CLIENT_ID) {
			final Button b = getButton(buttonId);
			b.setVisible(false);

			switch (state) {
				// Show the stack trace
				case DETAILS_HIDE:
					b.setText(getTranslation(ERROR_DIALOG_SHOW_DETAILS));

					state = DetailState.DETAILS_SHOW;
					txtDetails.setVisible(true);
					break;

				// Hide the stack trace
				case DETAILS_SHOW:
					b.setText(getTranslation(ERROR_DIALOG_HIDE_DETAILS));

					state = DetailState.DETAILS_HIDE;
					txtDetails.setVisible(false);
					break;
			}

			b.setVisible(true);
		}

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);
		newShell.setText(title);
		newShell.setImage(ImageCache.getImage(ImageCache.IMG_ERROR));
	}

}
