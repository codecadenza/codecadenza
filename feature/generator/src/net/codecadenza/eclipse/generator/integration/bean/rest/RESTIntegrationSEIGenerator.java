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
package net.codecadenza.eclipse.generator.integration.bean.rest;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.rest.BasicRESTMethodGenerator;
import net.codecadenza.eclipse.generator.integration.method.imp.rest.RESTMethodGeneratorFactory;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.integration.RESTIntegrationMethod;

/**
 * <p>
 * Generator for REST service end-point interfaces
 * </p>
 * <p>
 * Copyright 2017 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RESTIntegrationSEIGenerator extends AbstractJavaSourceGenerator {
	private final AbstractIntegrationBean integrationBean;

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public RESTIntegrationSEIGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getSEISourceFile());

		this.integrationBean = integrationBean;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("java.io");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public interface " + integrationBean.getInterfaceName() + " extends Serializable");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		integrationBean.getMethods().stream().map(RESTIntegrationMethod.class::cast).forEach(restMethod -> {
			final var b = new StringBuilder();
			final BasicRESTMethodGenerator restMethodGenerator = RESTMethodGeneratorFactory.getMethodGenerator(restMethod, this);
			final String signature = restMethodGenerator.getInterfaceMethodSignature(false, false);

			addImports(restMethodGenerator.getInterfaceImports());

			// Add the comment
			b.append(restMethodGenerator.createInterfaceComment());

			// Add the declaration
			b.append(getAnnotationForGeneratedElement());
			b.append(signature);
			b.append(";\n\n");

			addMethod(signature, b.toString());
		});
	}

}
