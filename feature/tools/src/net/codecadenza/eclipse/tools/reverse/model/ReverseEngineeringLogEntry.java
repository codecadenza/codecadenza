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

/**
 * <p>
 * Instances of this class represent log entries that are created while performing reverse engineering operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ReverseEngineeringLogEntry {
	public enum Status {
		INFO(1), WARNING(2), ERROR(3);

		private final int weightedValue;

		/**
		 * Constructor
		 * @param weightedValue
		 */
		Status(int weightedValue) {
			this.weightedValue = weightedValue;
		}

		/**
		 * @return the weighted value
		 */
		public int getWeightedValue() {
			return weightedValue;
		}
	}

	public enum Source {
		IMPORT, VALIDATION;
	}

	private final String message;
	private final Status status;
	private final Source source;

	/**
	 * Constructor
	 * @param message
	 * @param status
	 * @param source
	 */
	public ReverseEngineeringLogEntry(String message, Status status, Source source) {
		this.message = message;
		this.status = status;
		this.source = source;
	}

	/**
	 * @return the message
	 */
	public String getMessage() {
		return message;
	}

	/**
	 * @return the status
	 */
	public Status getStatus() {
		return status;
	}

	/**
	 * @return the source
	 */
	public Source getSource() {
		return source;
	}

}
