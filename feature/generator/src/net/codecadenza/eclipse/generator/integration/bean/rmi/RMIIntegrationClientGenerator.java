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
package net.codecadenza.eclipse.generator.integration.bean.rmi;

import static net.codecadenza.eclipse.shared.Constants.BASE_RMI_CLIENT_CLASS_NAME;
import static net.codecadenza.eclipse.shared.Constants.SUB_PACKAGE_INT_CLIENT;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationBean;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Generator for RMI integration clients
 * </p>
 * <p>
 * Copyright 2017 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class RMIIntegrationClientGenerator extends AbstractJavaSourceGenerator {
	private final AbstractIntegrationBean integrationBean;

	/**
	 * Constructor
	 * @param integrationBean
	 */
	public RMIIntegrationClientGenerator(AbstractIntegrationBean integrationBean) {
		super(integrationBean.getClientSourceFile());

		this.integrationBean = integrationBean;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("javax.naming");
		importPackage(integrationBean.getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class " + integrationBean.getClientClassName() + " extends " + BASE_RMI_CLIENT_CLASS_NAME);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.STRING, "EJB_NAME", "\"" + integrationBean.getName() + "!\"").create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		var identifier = integrationBean.getClientClassName() + "()";

		var b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Default constructor\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("this(null, null);\n");
		b.append("}\n\n");

		addConstructor(identifier, b.toString());

		identifier = integrationBean.getClientClassName() + "(String userName, String password)";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * Constructor\n");
		b.append(" * @param userName\n");
		b.append(" * @param password\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("super(userName, password);\n");
		b.append("}\n\n");

		addConstructor(identifier, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var identifier = "String getJNDIServiceName()";

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see " + integrationBean.getNamespace().toString() + SUB_PACKAGE_INT_CLIENT);
		b.append("." + BASE_RMI_CLIENT_CLASS_NAME + "#getJNDIServiceName()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public String getJNDIServiceName()\n");
		b.append("{\n");
		b.append("return NAMING_PREFIX + EJB_NAME + " + integrationBean.getInterfaceName() + ".class.getName();\n");
		b.append("}\n\n");

		addMethod(identifier, b.toString());

		identifier = integrationBean.getInterfaceName() + " getService()";

		b = new StringBuilder();
		b.append("/**\n");
		b.append(" * @return the service\n");
		b.append(" * @throws NamingException\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + " throws NamingException\n");
		b.append("{\n");
		b.append("return super.getService(" + integrationBean.getInterfaceName() + ".class);\n");
		b.append("}\n\n");

		addMethod(identifier, b.toString());
	}

}
