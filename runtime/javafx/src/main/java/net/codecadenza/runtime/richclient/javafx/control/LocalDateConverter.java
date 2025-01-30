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

import java.time.LocalDate;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import javafx.util.StringConverter;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;

/**
 * <p>
 * Converter for {@link LocalDate} objects. The converter uses the global date format.
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LocalDateConverter extends StringConverter<LocalDate> {
	private final FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
	private final DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern(userFormat.getDateFormat())
			.withZone(ZoneId.systemDefault());

	/*
	 * (non-Javadoc)
	 * @see javafx.util.StringConverter#toString(java.lang.Object)
	 */
	@Override
	public String toString(LocalDate date) {
		if (date != null)
			return dateFormatter.format(date);

		return "";
	}

	/*
	 * (non-Javadoc)
	 * @see javafx.util.StringConverter#fromString(java.lang.String)
	 */
	@Override
	public LocalDate fromString(String string) {
		if (string != null && !string.isEmpty())
			return LocalDate.parse(string, dateFormatter);

		return null;
	}

}
