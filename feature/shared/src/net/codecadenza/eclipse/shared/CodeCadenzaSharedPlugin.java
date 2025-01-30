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
package net.codecadenza.eclipse.shared;

import java.util.Locale;
import net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

/**
 * <p>
 * The plug-in class for common widgets
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaSharedPlugin extends AbstractUIPlugin {
	// The plug-in ID
	public static final String PLUGIN_ID = "net.codecadenza.eclipse.shared";

	// The shared instance
	private static CodeCadenzaSharedPlugin plugin;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start(org.osgi.framework.BundleContext)
	 */
	@Override
	public synchronized void start(BundleContext context) throws Exception {
		// Set the locale of the shared runtime library in order to use the same language in all plug-in components
		I18NEclipse.setLocale(Locale.ENGLISH);

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
	public static CodeCadenzaSharedPlugin getInstance() {
		return plugin;
	}

	/**
	 * Initialize the default preferences
	 */
	private void initializeDefaultPreferences() {
		final IPreferenceStore store = getPreferenceStore();
		store.setDefault(Constants.PREF_MAX_ROW_COUNT, 1000);
		store.setDefault(Constants.PREF_DATE_FORMAT, "dd.MM.yyyy");
		store.setDefault(Constants.PREF_TIME_FORMAT, "HH:mm:ss");
		store.setDefault(Constants.PREF_NUMBER_FORMAT, "0.00");
	}

}
