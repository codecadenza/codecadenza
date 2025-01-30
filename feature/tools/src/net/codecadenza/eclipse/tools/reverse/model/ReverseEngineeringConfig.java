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
package net.codecadenza.eclipse.tools.reverse.model;

import static net.codecadenza.eclipse.shared.Constants.PREF_ATTR_NAME_DATE_ON_PERSIST;
import static net.codecadenza.eclipse.shared.Constants.PREF_ATTR_NAME_DATE_ON_UPDATE;
import static net.codecadenza.eclipse.shared.Constants.PREF_ATTR_NAME_TRACK_VERSION;
import static net.codecadenza.eclipse.shared.Constants.PREF_DEEP_SEARCH;
import static net.codecadenza.eclipse.shared.Constants.PREF_END_WITH;
import static net.codecadenza.eclipse.shared.Constants.PREF_NOT_END_WITH;
import static net.codecadenza.eclipse.shared.Constants.PREF_NOT_START_WITH;
import static net.codecadenza.eclipse.shared.Constants.PREF_START_WITH;

import java.util.regex.Pattern;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * <p>
 * Configuration for reverse engineering
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ReverseEngineeringConfig {
	private static final String DEF_VERSION_ATTR_NAME = "version";
	private static final String DEF_SET_DATE_ON_PERSIST_ATTR_NAME = "creationDate";
	private static final String DEF_SET_DATE_ON_UPDATE_ATTR_NAME = "lastUpdate";
	private static final String SPLIT_REG_EX = "\\s+";
	private static final Pattern FILTER_WORD_PATTERN = Pattern.compile(SPLIT_REG_EX);

	private String startWith;
	private String endWith;
	private String notStartWith;
	private String notEndWith;
	private String dateOnPersistAttrName;
	private String dateOnUpdateAttrName;
	private String versionAttrName;
	private boolean deepSearchEnabled = true;

	/**
	 * @return a default configuration
	 */
	public static ReverseEngineeringConfig createDefaultFilter() {
		// Define default values for some configuration properties
		final IPreferenceStore store = CodeCadenzaToolsPlugin.getInstance().getPreferenceStore();
		store.setDefault(PREF_ATTR_NAME_TRACK_VERSION, DEF_VERSION_ATTR_NAME);
		store.setDefault(PREF_ATTR_NAME_DATE_ON_PERSIST, DEF_SET_DATE_ON_PERSIST_ATTR_NAME);
		store.setDefault(PREF_ATTR_NAME_DATE_ON_UPDATE, DEF_SET_DATE_ON_UPDATE_ATTR_NAME);
		store.setDefault(PREF_DEEP_SEARCH, true);

		// Initialize the configuration with values from the preference store
		final var config = new ReverseEngineeringConfig();
		config.startWith = store.getString(PREF_START_WITH);
		config.endWith = store.getString(PREF_END_WITH);
		config.notStartWith = store.getString(PREF_NOT_START_WITH);
		config.notEndWith = store.getString(PREF_NOT_END_WITH);
		config.versionAttrName = store.getString(PREF_ATTR_NAME_TRACK_VERSION);
		config.dateOnPersistAttrName = store.getString(PREF_ATTR_NAME_DATE_ON_PERSIST);
		config.dateOnUpdateAttrName = store.getString(PREF_ATTR_NAME_DATE_ON_UPDATE);
		config.setDeepSearchEnabled(store.getBoolean(PREF_DEEP_SEARCH));

		return config;
	}

	/**
	 * @param stringToTest
	 * @return true if the input matches the filter
	 */
	public boolean inputMatchesFilter(String stringToTest) {
		boolean match = false;

		if (stringToTest == null || stringToTest.isEmpty())
			return match;

		match = inputMatchesFilter(startWith, stringToTest, true, false);

		if (!match)
			return false;

		match = inputMatchesFilter(endWith, stringToTest, false, false);

		if (!match)
			return false;

		match = inputMatchesFilter(notStartWith, stringToTest, true, true);

		if (!match)
			return false;

		match = inputMatchesFilter(notEndWith, stringToTest, false, true);

		return match;
	}

	/**
	 * @param filter
	 * @param stringToTest
	 * @param searchAtBeginning
	 * @param invert
	 * @return true if the input matches the filter
	 */
	private boolean inputMatchesFilter(String filter, String stringToTest, boolean searchAtBeginning, boolean invert) {
		boolean match = false;

		if (filter != null && !filter.isEmpty()) {
			// Extract all filter words
			final String[] filterWords = FILTER_WORD_PATTERN.split(filter);

			for (final String item : filterWords) {
				if (searchAtBeginning)
					match = stringToTest.toLowerCase().startsWith(item.toLowerCase());
				else
					match = stringToTest.toLowerCase().endsWith(item.toLowerCase());

				if (invert) {
					// In case of an inverted test (e.g. input must not end with a specific suffix) an 'AND' logic is applied!
					if (match)
						return false;

					match = !match;
				}
				else if (match)
					break;
			}
		}
		else
			match = true;

		return match;
	}

	/**
	 * @return a list of words (delimited by whitespace characters) an input string must start with
	 */
	public String getStartWith() {
		return startWith;
	}

	/**
	 * @param startWith
	 */
	public void setStartWith(String startWith) {
		this.startWith = startWith;
	}

	/**
	 * @return a list of words (delimited by whitespace characters) an input string must end with
	 */
	public String getEndWith() {
		return endWith;
	}

	/**
	 * @param endWith
	 */
	public void setEndWith(String endWith) {
		this.endWith = endWith;
	}

	/**
	 * @return a list of words (delimited by whitespace characters) an input string must not start with
	 */
	public String getNotStartWith() {
		return notStartWith;
	}

	/**
	 * @param notStartWith
	 */
	public void setNotStartWith(String notStartWith) {
		this.notStartWith = notStartWith;
	}

	/**
	 * @return a list of words (delimited by whitespace characters) an input string must not end with
	 */
	public String getNotEndWith() {
		return notEndWith;
	}

	/**
	 * @param notEndWith
	 */
	public void setNotEndWith(String notEndWith) {
		this.notEndWith = notEndWith;
	}

	/**
	 * @return the name of date attributes that should be set automatically when creating a new object
	 */
	public String getDateOnPersistAttrName() {
		return dateOnPersistAttrName;
	}

	/**
	 * @param dateOnPersistAttrName
	 */
	public void setDateOnPersistAttrName(String dateOnPersistAttrName) {
		this.dateOnPersistAttrName = dateOnPersistAttrName;
	}

	/**
	 * @return the name of date attributes that should be set automatically when saving an update
	 */
	public String getDateOnUpdateAttrName() {
		return dateOnUpdateAttrName;
	}

	/**
	 * @param dateOnUpdateAttrName
	 */
	public void setDateOnUpdateAttrName(String dateOnUpdateAttrName) {
		this.dateOnUpdateAttrName = dateOnUpdateAttrName;
	}

	/**
	 * @return the name of version attributes
	 */
	public String getVersionAttrName() {
		return versionAttrName;
	}

	/**
	 * @param versionAttrName
	 */
	public void setVersionAttrName(String versionAttrName) {
		this.versionAttrName = versionAttrName;
	}

	/**
	 * @return true if a database table filter operation should include tables that are referenced by foreign keys
	 */
	public boolean isDeepSearchEnabled() {
		return deepSearchEnabled;
	}

	/**
	 * @param deepSearchEnabled
	 */
	public void setDeepSearchEnabled(boolean deepSearchEnabled) {
		this.deepSearchEnabled = deepSearchEnabled;
	}

}
