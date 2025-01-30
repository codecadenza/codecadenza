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

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import jakarta.ws.rs.core.MediaType;
import java.util.List;
import org.junit.jupiter.api.Test;

/**
 * <p>
 * Test the functionality of the {@link AcceptHeaderParser}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
class AcceptHeaderParserTest {
	@Test
	void testParseMultipleMediaTypes() {
		final var mediaTypeList = MediaType.TEXT_PLAIN + "," + MediaType.APPLICATION_JSON + "," + MediaType.APPLICATION_XML;

		final List<MediaType> mediaTypes = AcceptHeaderParser.parse(mediaTypeList);

		assertEquals(3, mediaTypes.size());
		assertEquals(MediaType.TEXT_PLAIN_TYPE, mediaTypes.get(0));
		assertEquals(MediaType.APPLICATION_JSON_TYPE, mediaTypes.get(1));
		assertEquals(MediaType.APPLICATION_XML_TYPE, mediaTypes.get(2));
	}

	@Test
	void testIgnoreIncorrectFormat() {
		final var acceptHeader = "text-plain";

		final List<MediaType> mediaTypes = AcceptHeaderParser.parse(acceptHeader);

		assertTrue(mediaTypes.isEmpty());
	}

	@Test
	void testCanProcessMediaTypeWithParameters() {
		final var acceptHeader = MediaType.TEXT_HTML + ";level=2; q=0.4";

		final List<MediaType> mediaTypes = AcceptHeaderParser.parse(acceptHeader);

		assertEquals(1, mediaTypes.size());
		assertEquals(MediaType.TEXT_HTML_TYPE, mediaTypes.get(0));
	}

}
