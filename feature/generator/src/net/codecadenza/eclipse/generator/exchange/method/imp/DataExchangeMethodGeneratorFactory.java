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
package net.codecadenza.eclipse.generator.exchange.method.imp;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.exchange.method.AbstractExchangeMethodGenerator;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;

/**
 * <p>
 * Factory class for data exchange method generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DataExchangeMethodGeneratorFactory {
	/**
	 * Private constructor
	 */
	private DataExchangeMethodGeneratorFactory() {

	}

	/**
	 * @param generator
	 * @param method
	 * @return the method generator based on the selected content and method type
	 * @throws IllegalStateException if an implementation for the given content and method type is not available
	 */
	public static AbstractExchangeMethodGenerator getMethodGenerator(AbstractJavaSourceGenerator generator,
			DataExchangeMethod method) {
		final ContentTypeEnumeration contentType = method.getContentType();
		final DataExchangeMethodTypeEnumeration methodType = method.getMethodType();

		if (contentType == ContentTypeEnumeration.XML) {
			if (methodType == DataExchangeMethodTypeEnumeration.EXPORT)
				return new XMLExportMethodGenerator(generator, method);

			return new XMLImportMethodGenerator(generator, method);
		}
		else if (contentType == ContentTypeEnumeration.EXCEL97) {
			if (methodType == DataExchangeMethodTypeEnumeration.EXPORT)
				return new Excel97ExportMethodGenerator(generator, method);

			return new ExcelImportMethodGenerator(generator, method);
		}
		else if (contentType == ContentTypeEnumeration.EXCEL2007) {
			if (methodType == DataExchangeMethodTypeEnumeration.EXPORT)
				return new Excel2007ExportMethodGenerator(generator, method);

			return new ExcelImportMethodGenerator(generator, method);
		}
		else if (contentType == ContentTypeEnumeration.CSV) {
			if (methodType == DataExchangeMethodTypeEnumeration.EXPORT)
				return new CSVExportMethodGenerator(generator, method);

			return new CSVImportMethodGenerator(generator, method);
		}
		else if (contentType == ContentTypeEnumeration.JSON) {
			if (methodType == DataExchangeMethodTypeEnumeration.EXPORT)
				return new JSonExportMethodGenerator(generator, method);

			return new JSonImportMethodGenerator(generator, method);
		}

		final var msg = "An exchange method generator for content type '" + contentType + "' and method type '" + methodType
				+ "' is not available!";

		throw new IllegalStateException(msg);
	}

}
