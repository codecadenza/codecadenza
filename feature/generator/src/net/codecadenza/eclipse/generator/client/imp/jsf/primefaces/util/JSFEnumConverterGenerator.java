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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util;

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN_TYPE;
import static net.codecadenza.eclipse.shared.Constants.CONVERSION_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_CONVERTER;

import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for the conversion of enumeration literals in a JSF application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFEnumConverterGenerator extends AbstractJavaSourceGenerator {
	private final JavaEnum javaEnum;
	private final Project project;

	/**
	 * Constructor
	 * @param javaEnum
	 */
	public JSFEnumConverterGenerator(JavaEnum javaEnum) {
		this.javaEnum = javaEnum;
		this.project = javaEnum.getNamespace().getProject();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final var converterName = javaEnum.getName() + CONVERSION_SUFFIX;
		final String converterPackage = project.getClientNamespace().toString() + PACK_CLIENT_CONVERTER;

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, converterName, converterPackage);
		javaFile.setComment("Converter for enum fields of type {@link " + javaEnum.getName() + "}");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + USER_SESSION_BEAN_TYPE);
		importPackage(project.getClientNamespace().toString());
		importPackage(javaEnum.getNamespace().toString());
		importPackage("jakarta.faces.context");
		importPackage("jakarta.faces.convert");
		importPackage("jakarta.faces.component");
		importPackage("jakarta.inject");
		importPackage("java.util");
		importPackage("java.util.stream");

		if (project.isJakartaEEApplication())
			importPackage("jakarta.enterprise.context");
		else
			importClass("org.springframework.web.context.annotation.SessionScope");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Named(\"" + JSFGeneratorUtil.createManagedBeanName(javaEnum.getName() + CONVERSION_SUFFIX) + "\")\n");

		if (project.isJakartaEEApplication())
			b.append("@RequestScoped\n");
		else
			b.append("@SessionScope\n");

		b.append("public class " + javaEnum.getName() + CONVERSION_SUFFIX);
		b.append(" implements Converter<" + javaEnum.getName() + ">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateField(USER_SESSION_BEAN_TYPE, USER_SESSION_BEAN).inject().create();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		var b = new StringBuilder();
		var methodSignature = javaEnum.getName()
				+ " getAsObject(FacesContext facesContext, UIComponent component, String submittedValue)";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.faces.convert.Converter#");
		b.append("getAsObject(jakarta.faces.context.FacesContext, jakarta.faces.component.UIComponent, java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final ResourceBundle bundle = ResourceBundle.getBundle(DEFAULT_BUNDLE_NAME, ");
		b.append(USER_SESSION_BEAN + ".getLocale());\n\n");
		b.append("return Stream.of(" + javaEnum.getName() + ".values()).filter(value -> bundle.getString(\"");
		b.append(javaEnum.getName().toLowerCase() + "_\" ");
		b.append("+ value.name().toLowerCase()).equals(submittedValue)).findFirst().orElse(null);\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());

		b = new StringBuilder();
		methodSignature = "String getAsString(FacesContext facesContext, UIComponent component, " + javaEnum.getName() + " value)";

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.faces.convert.Converter#");
		b.append("getAsString(jakarta.faces.context.FacesContext, jakarta.faces.component.UIComponent, java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("final ResourceBundle bundle = ResourceBundle.getBundle(DEFAULT_BUNDLE_NAME, ");
		b.append(USER_SESSION_BEAN + ".getLocale());\n\n");
		b.append("return bundle.getString(\"" + javaEnum.getName().toLowerCase() + "_\" + value.name().toLowerCase());\n");
		b.append("}\n\n");

		addMethod(methodSignature, b.toString());
	}

}
