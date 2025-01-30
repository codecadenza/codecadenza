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
package net.codecadenza.eclipse.tools;

import static net.codecadenza.eclipse.shared.Constants.PREF_DATE_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PREF_MAX_ROW_COUNT;
import static net.codecadenza.eclipse.shared.Constants.PREF_NUMBER_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PREF_TIME_FORMAT;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * <p>
 * The plug-in class for common tools
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaToolsPlugin extends AbstractUIPlugin {
	// The plug-in ID
	public static final String PLUGIN_ID = "net.codecadenza.eclipse.tools";

	// The shared instance
	private static CodeCadenzaToolsPlugin plugin;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public synchronized void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;

		initializeDefaultPreferences();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	@Override
	public synchronized void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * @return the shared instance
	 */
	public static CodeCadenzaToolsPlugin getInstance() {
		return plugin;
	}

	/**
	 * Log the error and open a dialog in order to inform the user that an error has occurred
	 * @param t
	 */
	public void handleInternalError(Throwable t) {
		final Shell shell = Display.getCurrent().getActiveShell();

		// An IllegalStateException usually indicates that an internal validation error has occurred!
		if (t instanceof IllegalStateException) {
			MessageDialog.openWarning(shell, "CodeCadenza - Validation Error", t.getMessage());
			return;
		}

		MessageDialog.openError(shell, "CodeCadenza - Internal Error", "An internal error has occurred! Message: " + t.getMessage());
		logError(t);
	}

	/**
	 * @param throwable
	 */
	public void logError(Throwable throwable) {
		logError(null, throwable);
	}

	/**
	 * @param error
	 * @param throwable
	 */
	public void logError(String error, Throwable throwable) {
		if (error == null && throwable != null)
			error = throwable.getMessage();

		getLog().log(new Status(IStatus.ERROR, PLUGIN_ID, IStatus.OK, error, throwable));
		debug(error, throwable);
	}

	/**
	 * @param message
	 */
	public void logInfo(String message) {
		logInfo(message, null);
	}

	/**
	 * @param message
	 * @param throwable
	 */
	public void logInfo(String message, Throwable throwable) {
		if (message == null && throwable != null)
			message = throwable.getMessage();

		getLog().log(new Status(IStatus.INFO, PLUGIN_ID, IStatus.OK, message, throwable));
		debug(message, throwable);
	}

	/**
	 * @param message
	 * @param throwable
	 */
	private void debug(String message, Throwable throwable) {
		if (!isDebugging())
			return;

		if (message != null)
			System.err.println(message);

		if (throwable != null)
			throwable.printStackTrace();
	}

	/**
	 * Initialize the default preferences
	 */
	private void initializeDefaultPreferences() {
		final IPreferenceStore store = getPreferenceStore();
		store.setDefault(PREF_MAX_ROW_COUNT, 1000);
		store.setDefault(PREF_DATE_FORMAT, "dd.MM.yyyy");
		store.setDefault(PREF_TIME_FORMAT, "HH:mm:ss");
		store.setDefault(PREF_NUMBER_FORMAT, "0.00");
	}

}
