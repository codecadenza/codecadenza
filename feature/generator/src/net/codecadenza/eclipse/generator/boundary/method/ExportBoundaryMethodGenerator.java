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
package net.codecadenza.eclipse.generator.boundary.method;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.common.IMethodGeneratorUtility;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.DirectExchangeMode;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;

/**
 * <p>
 * Generator for boundary methods that invoke data export methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ExportBoundaryMethodGenerator extends BasicBoundaryMethodGenerator {
	protected DataExchangeMethod exchangeMethod;
	protected DataExchangeServiceBean exchangeService;
	protected boolean directMode;

	/**
	 * Constructor
	 * @param method
	 * @param utility
	 * @param parentGenerator
	 */
	public ExportBoundaryMethodGenerator(BoundaryMethod method, IMethodGeneratorUtility utility,
			AbstractJavaSourceGenerator parentGenerator) {
		super(method, utility, parentGenerator);

		this.exchangeMethod = (DataExchangeMethod) method.getServiceMethod();
		this.exchangeService = exchangeMethod.getDataExchangeServiceBean();
		this.directMode = exchangeMethod.getExchangeMode() instanceof DirectExchangeMode;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		b.append(method.generateBeginOfJavadocComment(true));

		if (exchangeMethod.returnsPath())
			b.append(" * @return the absolute path of the generated file\n");
		else if (exchangeMethod.returnsContent())
			b.append(" * @return the generated content\n");
		else if (directMode)
			b.append(" * @return the result object\n");

		b.append(" * @throws DataExportException if export operation has failed\n");
		b.append(" */\n");

		return b.toString();
	}

	/**
	 * @param param
	 * @return the default value for a given parameter
	 */
	private String setDefaultParamValue(MethodParameter param) {
		if (param.getType().isPrimitive()) {
			if (param.getType().isType(JavaType.INT))
				return "1";
			else if (param.getType().isType(JavaType.LONG))
				return "1L";
			else if (param.getType().isType(JavaType.DOUBLE))
				return "1.0";
			else if (param.getType().isType(JavaType.FLOAT))
				return "1.0F";
			else if (param.getType().isType(JavaType.BOOL))
				return "false";
			else if (param.getType().isType(JavaType.CHAR))
				return "'\\u0000'";
		}

		return "null";
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();
		final String serviceName = exchangeService.getLowerCaseName();

		if (exchangeMethod.returnsContent() || exchangeMethod.returnsPath() || directMode)
			b.append("return ");

		b.append(serviceName + "." + exchangeMethod.getName() + "(");

		boolean isFirstParam = true;

		for (final MethodParameter exchangeParam : exchangeMethod.getMethodParameters()) {
			final MethodParameter filterParam;

			// In case of DIRECT mode every boundary method parameter is mapped to a corresponding data exchange method parameter!
			if (directMode)
				filterParam = exchangeParam;
			else
				filterParam = exchangeMethod.getSingleObjectFilterParam();

			if (isFirstParam)
				isFirstParam = false;
			else
				b.append(", ");

			if (filterParam != null) {
				for (final MethodParameter boundaryParam : method.getMethodParameters())
					if (boundaryParam.getName().equals(filterParam.getName()) && boundaryParam.getType().equals(filterParam.getType())) {
						b.append(boundaryParam.getName());
						break;
					}
			}
			else
				b.append(setDefaultParamValue(exchangeParam));
		}

		b.append(");\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());
		imports.add("import net.codecadenza.runtime.exchange.*;");
		imports.add("import " + exchangeService.getNamespace().toString() + ".*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.boundary.method.BasicBoundaryMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final var imports = new HashSet<>(super.getImports());
		imports.add("import net.codecadenza.runtime.exchange.*;");

		return imports;
	}

}
