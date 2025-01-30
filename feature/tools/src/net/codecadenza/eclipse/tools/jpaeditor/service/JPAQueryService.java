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
package net.codecadenza.eclipse.tools.jpaeditor.service;

import static net.codecadenza.eclipse.shared.Constants.PREF_DATE_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PREF_NUMBER_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PREF_TIME_FORMAT;

import jakarta.persistence.EntityManager;
import java.math.BigDecimal;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;
import org.eclipse.jface.preference.IPreferenceStore;

/**
 * <p>
 * Utility class that provides a method for executing JPA queries
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JPAQueryService {
	public static final String NULL_VALUE = "[null]";
	public static final String EMPTY_STRING = "[empty]";
	private static final String FROM = " from ";
	private static final String SELECT = "select ";

	/**
	 * Prevent instantiation
	 */
	private JPAQueryService() {

	}

	/**
	 * @param jpaQL
	 * @param em
	 * @param maxRowcount
	 * @return the query result set
	 */
	public static JPAQueryResultSet executeQuery(String jpaQL, EntityManager em, int maxRowcount) {
		// Get the preference store
		final IPreferenceStore store = net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin.getInstance().getPreferenceStore();

		// Initialize the formatters for date and numeric values
		final var dateTimeFormatter = new SimpleDateFormat(
				store.getString(PREF_DATE_FORMAT) + " " + store.getString(PREF_TIME_FORMAT));
		final var decimalFormatter = new DecimalFormat(store.getString(PREF_NUMBER_FORMAT));
		final var rs = new JPAQueryResultSet();
		final String selectClause;
		boolean flat = false;

		jpaQL = jpaQL.replace("\n", " ");
		jpaQL = jpaQL.replace("\t", " ");
		jpaQL = jpaQL.trim();
		List<?> result = null;

		if (!jpaQL.toLowerCase().startsWith(SELECT))
			throw new IllegalStateException("Query must start with 'select'!");

		if (jpaQL.toLowerCase().contains(FROM))
			selectClause = jpaQL.substring(0, jpaQL.toLowerCase().indexOf(FROM));
		else
			throw new IllegalStateException("Query doesn't contain 'from' clause!");

		if (maxRowcount != 0)
			result = em.createQuery(jpaQL).setMaxResults(maxRowcount).getResultList();
		else
			result = em.createQuery(jpaQL).getResultList();

		final String[] fields = selectClause.substring(SELECT.length()).trim().split(",");

		for (int i = 0; i < fields.length; i++)
			fields[i] = fields[i].trim();

		boolean headersSet = false;

		for (final Object value : result) {
			final var ds = new ArrayList<String>();

			if (value != null && value.getClass() == Object[].class) {
				final var values = (Object[]) value;
				flat = true;

				for (int a = 0; a < values.length; a++) {
					final Object o = values[a];

					if ((!headersSet))
						rs.getHeader().add(fields[a]);

					if (o == null)
						ds.add(NULL_VALUE);
					else if (o.getClass() == Integer.class || o.getClass() == int.class || o.getClass() == Long.class
							|| o.getClass() == long.class || o.getClass() == UUID.class)
						ds.add(o.toString());
					else if (o.getClass() == Double.class || o.getClass() == double.class) {
						final var doubleValue = (Double) o;
						ds.add(decimalFormatter.format(doubleValue));
					}
					else if (o.getClass() == BigDecimal.class) {
						final var bigDecimalValue = (BigDecimal) o;
						ds.add(decimalFormatter.format(bigDecimalValue));
					}
					else if (o.getClass() == Float.class || o.getClass() == float.class) {
						final var floatValue = (Float) o;
						ds.add(decimalFormatter.format(floatValue));
					}
					else if (o.getClass() == String.class) {
						var stringValue = (String) o;

						if (!stringValue.isEmpty()) {
							// Return only the first line!
							if (stringValue.contains("\n"))
								stringValue = stringValue.substring(0, stringValue.indexOf('\n')) + "...";
							else if (stringValue.contains("\r"))
								stringValue = stringValue.substring(0, stringValue.indexOf('\r')) + "...";
						}
						else
							stringValue = EMPTY_STRING;

						ds.add(stringValue);
					}
					else if (o.getClass() == boolean.class || o.getClass() == Boolean.class) {
						final var boolValue = (Boolean) o;
						ds.add(boolValue.toString());
					}
					else if (o.getClass() == java.sql.Time.class) {
						final var in = new Date(((java.sql.Time) o).getTime());
						ds.add(dateTimeFormatter.format(in));
					}
					else if (o.getClass() == java.sql.Timestamp.class) {
						final var in = new Date(((java.sql.Timestamp) o).getTime());
						ds.add(dateTimeFormatter.format(in));
					}
					else if (o instanceof final Enum<?> enumeration)
						ds.add(enumeration.name());
					else
						throw new IllegalStateException("Objects and attributes cannot be entered in one select statement!");
				}
			}
			else {
				flat = true;

				if ((!headersSet))
					rs.getHeader().add(fields[0]);

				if (value == null)
					ds.add(NULL_VALUE);
				else if (value.getClass() == Integer.class || value.getClass() == int.class || value.getClass() == Long.class
						|| value.getClass() == long.class || value.getClass() == UUID.class)
					ds.add(value.toString());
				else if (value.getClass() == Double.class || value.getClass() == double.class) {
					final var doubleValue = (Double) value;
					ds.add(decimalFormatter.format(doubleValue));
				}
				else if (value.getClass() == Float.class || value.getClass() == float.class) {
					final var floatValue = (Float) value;
					ds.add(decimalFormatter.format(floatValue));
				}
				else if (value.getClass() == BigDecimal.class) {
					final var bigDecimalValue = (BigDecimal) value;
					ds.add(decimalFormatter.format(bigDecimalValue));
				}
				else if (value.getClass() == String.class) {
					var stringValue = (String) value;

					if (!stringValue.isEmpty()) {
						// Return only the first line!
						if (stringValue.contains("\n"))
							stringValue = stringValue.substring(0, stringValue.indexOf('\n')) + "...";
						else if (stringValue.contains("\r"))
							stringValue = stringValue.substring(0, stringValue.indexOf('\r')) + "...";
					}
					else
						stringValue = EMPTY_STRING;

					ds.add(stringValue);
				}
				else if (value.getClass() == boolean.class || value.getClass() == Boolean.class) {
					final var boolValue = (Boolean) value;
					ds.add(boolValue.toString());
				}
				else if (value.getClass() == java.sql.Time.class) {
					final var in = new Date(((java.sql.Time) value).getTime());
					ds.add(dateTimeFormatter.format(in));
				}
				else if (value.getClass() == java.sql.Timestamp.class) {
					final var in = new Date(((java.sql.Timestamp) value).getTime());
					ds.add(dateTimeFormatter.format(in));
				}
				else if (value instanceof Enum) {
					final var in = (Enum<?>) value;
					ds.add(in.name());
				}
				else {
					flat = false;

					rs.getRowObjects().add(value);

					ds.add(value.getClass().getName().substring(value.getClass().getName().lastIndexOf('.') + 1));
				}
			}

			headersSet = true;
			rs.setFlat(flat);
			rs.getRows().add(ds);
		}

		return rs;
	}

}
