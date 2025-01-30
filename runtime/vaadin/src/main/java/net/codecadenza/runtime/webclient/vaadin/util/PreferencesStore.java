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
package net.codecadenza.runtime.webclient.vaadin.util;

import jakarta.annotation.PostConstruct;
import jakarta.enterprise.context.SessionScoped;
import jakarta.inject.Inject;
import jakarta.servlet.http.Cookie;
import java.io.Serializable;
import java.lang.invoke.MethodHandles;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Locale;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Utility class for handling the user preferences
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@SessionScoped
public class PreferencesStore implements Serializable {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = 3138355201622339189L;

	private static final String DEF_DATETIME_FORMAT = "dd.MM.yyyy HH:mm:ss";
	private static final String DEF_DATE_FORMAT = "dd.MM.yyyy";
	private static final String DEF_NUMBER_FORMAT = "###,###,##0.00";
	private static final String DEF_LANGUAGE = Locale.ENGLISH.toLanguageTag();
	private static final String KEY_DATE_FORMAT = "dateformat";
	private static final String KEY_DATETIME_FORMAT = "datetimeformat";
	private static final String KEY_NUMBER_FORMAT = "numberformat";
	private static final String KEY_LANGUAGE = "language";

	private final CookieManager cookieManager;
	private final SessionStore sessionStore;

	/**
	 * Constructor
	 */
	public PreferencesStore() {
		this.cookieManager = null;
		this.sessionStore = null;
	}

	/**
	 * Constructor
	 * @param cookieManager
	 * @param sessionStore
	 */
	@Inject
	public PreferencesStore(CookieManager cookieManager, SessionStore sessionStore) {
		this.cookieManager = cookieManager;
		this.sessionStore = sessionStore;
	}

	/**
	 * Initialize the preferences
	 */
	@PostConstruct
	public void init() {
		logger.debug("Initialize user preferences");

		// In any case the store should be filled with proper values
		sessionStore.putValue(KEY_DATE_FORMAT, DEF_DATE_FORMAT);
		sessionStore.putValue(KEY_DATETIME_FORMAT, DEF_DATETIME_FORMAT);
		sessionStore.putValue(KEY_NUMBER_FORMAT, DEF_NUMBER_FORMAT);
		sessionStore.putValue(KEY_LANGUAGE, DEF_LANGUAGE);

		try {
			// Load all user preferences from respective cookies
			final Cookie cDateFormat = cookieManager.getCookieByName(KEY_DATE_FORMAT);
			final Cookie cDateTimeFormat = cookieManager.getCookieByName(KEY_DATETIME_FORMAT);
			final Cookie cNumberFormat = cookieManager.getCookieByName(KEY_NUMBER_FORMAT);
			final Cookie cLanguage = cookieManager.getCookieByName(KEY_LANGUAGE);

			if (cDateFormat != null && cDateFormat.getValue() != null && !cDateFormat.getValue().isEmpty())
				sessionStore.putValue(KEY_DATE_FORMAT, URLDecoder.decode(cDateFormat.getValue(), StandardCharsets.UTF_8));

			if (cDateTimeFormat != null && cDateTimeFormat.getValue() != null && !cDateTimeFormat.getValue().isEmpty())
				sessionStore.putValue(KEY_DATETIME_FORMAT, URLDecoder.decode(cDateTimeFormat.getValue(), StandardCharsets.UTF_8));

			if (cNumberFormat != null && cNumberFormat.getValue() != null && !cNumberFormat.getValue().isEmpty())
				sessionStore.putValue(KEY_NUMBER_FORMAT, URLDecoder.decode(cNumberFormat.getValue(), StandardCharsets.UTF_8));

			if (cLanguage != null && cLanguage.getValue() != null && !cLanguage.getValue().isEmpty())
				sessionStore.putValue(KEY_LANGUAGE, URLDecoder.decode(cLanguage.getValue(), StandardCharsets.UTF_8));
		}
		catch (final Exception e) {
			logger.error("Initialization of user preferences failed!", e);
		}
	}

	/**
	 * Save all user preferences in respective cookies
	 */
	public void save() {
		cookieManager.saveCookie(KEY_DATE_FORMAT, getDateFormat());
		cookieManager.saveCookie(KEY_DATETIME_FORMAT, getDateTimeFormat());
		cookieManager.saveCookie(KEY_NUMBER_FORMAT, getNumberFormat());
		cookieManager.saveCookie(KEY_LANGUAGE, getLanguage());
	}

	/**
	 * @return the date format
	 */
	public String getDateFormat() {
		return sessionStore.getValue(KEY_DATE_FORMAT);
	}

	/**
	 * @param dateFormat
	 */
	public void setDateFormat(String dateFormat) {
		sessionStore.putValue(KEY_DATE_FORMAT, dateFormat);
	}

	/**
	 * @return the date time format
	 */
	public String getDateTimeFormat() {
		return sessionStore.getValue(KEY_DATETIME_FORMAT);
	}

	/**
	 * @param dateTimeFormat
	 */
	public void setDateTimeFormat(String dateTimeFormat) {
		sessionStore.putValue(KEY_DATETIME_FORMAT, dateTimeFormat);
	}

	/**
	 * @return the number format
	 */
	public String getNumberFormat() {
		return sessionStore.getValue(KEY_NUMBER_FORMAT);
	}

	/**
	 * @param numberFormat
	 */
	public void setNumberFormat(String numberFormat) {
		sessionStore.putValue(KEY_NUMBER_FORMAT, numberFormat);
	}

	/**
	 * @return the language
	 */
	public String getLanguage() {
		return sessionStore.getValue(KEY_LANGUAGE);
	}

	/**
	 * @param language
	 */
	public void setLanguage(String language) {
		sessionStore.putValue(KEY_LANGUAGE, language);
	}

}
