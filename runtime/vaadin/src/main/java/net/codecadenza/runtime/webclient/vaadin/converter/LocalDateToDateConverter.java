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
package net.codecadenza.runtime.webclient.vaadin.converter;

import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Date;

/**
 * <p>
 * {@link LocalDate} to {@link Date} converter
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class LocalDateToDateConverter extends com.vaadin.flow.data.converter.LocalDateToDateConverter {
	private static final long serialVersionUID = 1839917649063747547L;

	/**
	 * Constructor
	 */
	public LocalDateToDateConverter() {
		super(ZoneId.systemDefault());
	}

	/**
	 * Constructor
	 * @param zoneId
	 */
	public LocalDateToDateConverter(ZoneId zoneId) {
		super(zoneId);
	}

}
