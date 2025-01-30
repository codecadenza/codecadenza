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
package net.codecadenza.eclipse.testing.domain;

/**
 * <p>
 * Objects of this class represent data exchange methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataExchangeMethod {
	private final DomainObject domainObject;
	private final ContentType contentType;
	private final boolean processSingleObject;
	private final boolean importData;

	/**
	 * Constructor
	 * @param domainObject
	 * @param contentType
	 * @param processSingleObject
	 * @param importData
	 */
	public DataExchangeMethod(DomainObject domainObject, ContentType contentType, boolean processSingleObject, boolean importData) {
		this.domainObject = domainObject;
		this.contentType = contentType;
		this.processSingleObject = processSingleObject;
		this.importData = importData;
	}

	/**
	 * @return the domain object
	 */
	public DomainObject getDomainObject() {
		return domainObject;
	}

	/**
	 * @return the content type
	 */
	public ContentType getContentType() {
		return contentType;
	}

	/**
	 * @return true if a single object should be processed
	 */
	public boolean isProcessSingleObject() {
		return processSingleObject;
	}

	/**
	 * @return true if the method should import data
	 */
	public boolean isImportData() {
		return importData;
	}

}
