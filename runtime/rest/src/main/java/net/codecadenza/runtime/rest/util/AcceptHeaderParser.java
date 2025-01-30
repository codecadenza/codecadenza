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
package net.codecadenza.runtime.rest.util;

import jakarta.ws.rs.core.MediaType;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

/**
 * <p>
 * Utility class for parsing the value of a HTTP 'Accept' header
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class AcceptHeaderParser {
	/**
	 * Prevent instantiation
	 */
	private AcceptHeaderParser() {

	}

	/**
	 * Parse the given string (e.g. 'application/xml, image/gif') and extract all {@link MediaType media types}
	 * @param headerValue the header value that should be parsed
	 * @return a list of {@link MediaType media types}
	 */
	public static List<MediaType> parse(String headerValue) {
		if (headerValue == null || headerValue.isEmpty())
			return Collections.emptyList();

		final var mediaTypes = new ArrayList<MediaType>();

		for (final String token : headerValue.split(",")) {
			final String[] typeAndSubtype = token.split("/");

			if (typeAndSubtype.length == 2) {
				final String type = typeAndSubtype[0].trim();
				final String subType = typeAndSubtype[1].trim();

				if (subType.contains(";"))
					// Omit all parameters!
					mediaTypes.add(new MediaType(type, subType.substring(0, subType.indexOf(';')).trim()));
				else
					mediaTypes.add(new MediaType(type, subType));
			}
		}

		return mediaTypes;
	}

}
