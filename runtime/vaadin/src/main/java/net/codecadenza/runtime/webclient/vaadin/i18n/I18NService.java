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

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import java.util.Locale;
import java.util.ResourceBundle;
import net.codecadenza.runtime.webclient.vaadin.util.PreferencesStore;

/**
 * <p>
 * Service that is responsible for translating text fragments
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@SessionScoped
public class I18NService extends AbstractI18NService {
	private static final long serialVersionUID = 8688185311740381782L;
	private static final String BUNDLE_NAME = "translation";

	private final PreferencesStore preferences;
	private transient ResourceBundle bundle;

	/**
	 * Constructor
	 */
	public I18NService() {
		this.preferences = null;
	}

	/**
	 * Constructor
	 * @param preferences
	 */
	@Inject
	public I18NService(PreferencesStore preferences) {
		this.preferences = preferences;
	}

	/**
	 * Initialize the resource bundle by loading the selected language from the preferences store
	 */
	@PostConstruct
	public void init() {
		locale = Locale.forLanguageTag(preferences.getLanguage());
		bundle = ResourceBundle.getBundle(BUNDLE_NAME, locale);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.i18n.AbstractI18NService#getBundle()
	 */
	@Override
	protected ResourceBundle getBundle() {
		return bundle;
	}

}
