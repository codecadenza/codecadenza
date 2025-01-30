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
package net.codecadenza.runtime.webclient.vaadin.i18n;

import java.io.Serializable;
import java.util.Locale;
import java.util.ResourceBundle;
import net.codecadenza.runtime.i18n.I18N;

/**
 * <p>
 * Abstract base class for the translation of text fragments
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractI18NService implements Serializable {
	private static final long serialVersionUID = -2416720427742360244L;

	protected Locale locale;

	/**
	 * Default constructor
	 */
	protected AbstractI18NService() {
		this.locale = Locale.ENGLISH;
	}

	/**
	 * Constructor
	 * @param locale
	 */
	protected AbstractI18NService(Locale locale) {
		this.locale = locale;
	}

	/**
	 * @param locale
	 */
	public void setLocale(Locale locale) {
		this.locale = locale;
	}

	/**
	 * @return the locale
	 */
	public Locale getLocale() {
		return locale;
	}

	/**
	 * Template method that determines the bundle
	 * @return the {@link ResourceBundle} to be used
	 */
	protected abstract ResourceBundle getBundle();

	/**
	 * @param key
	 * @param params
	 * @return the translation text by using the given key
	 */
	public String getTranslation(String key, Object... params) {
		return I18N.getTranslation(getBundle(), key, params);
	}

	/**
	 * @param key
	 * @return the translation for a field label by using the given key
	 */
	public String getTranslationForFieldLabel(String key) {
		return I18N.getTranslationForFieldLabel(getBundle(), key);
	}

}
