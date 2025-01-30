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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.converter;

import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN;
import static net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.JSFConstants.USER_SESSION_BEAN_TYPE;
import static net.codecadenza.eclipse.shared.Constants.CONVERSION_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_CONVERTER;

import java.util.Optional;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.util.JSFGeneratorUtil;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.BuildArtifactType;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * Generator for JSF converters
 * </p>
 * <p>
 * Copyright 2017 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFConverterGenerator extends AbstractJavaSourceGenerator {
	private static final String CONSTANT_NBSP = "NON_BREAKING_SPACE";

	private final DTOBean modelObject;
	private final Project project;
	private final boolean isMandated;
	private final DTOBeanAttribute displayAttr;
	private final BoundaryBean boundaryBean;

	/**
	 * Constructor
	 * @param modelObject
	 */
	public JSFConverterGenerator(DTOBean modelObject) {
		this.modelObject = modelObject;
		this.project = modelObject.getNamespace().getProject();
		this.displayAttr = modelObject.getDisplayAttribute();
		this.isMandated = modelObject.getDomainObject().isMandated() && displayAttr != null;
		this.boundaryBean = project.getBoundaryByDomainObject(modelObject.getDomainObject());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#initJavaFile()
	 */
	@Override
	protected JavaFile initJavaFile() {
		final var converterName = modelObject.getModelClassName() + "Converter";
		final String converterPackage = project.getClientNamespace().toString() + PACK_CLIENT_CONVERTER;

		final var javaFile = new JavaFile(project, BuildArtifactType.GUI, converterName, converterPackage);
		javaFile.setComment("JSF converter");

		return javaFile;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importPackage("jakarta.faces.context");
		importPackage("jakarta.faces.convert");
		importPackage("jakarta.faces.component");
		importPackage("jakarta.inject");

		if (project.isJakartaEEApplication())
			importPackage("jakarta.enterprise.context");
		else
			importClass("org.springframework.web.context.annotation.RequestScope");

		if (isMandated)
			importPackage(project.getClientNamespace().toString());

		if (project.isBoundaryMode())
			importPackage(modelObject.getNamespace().toString());
		else
			importPackage(modelObject.getDomainObject().getNamespace().toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("@Named(\"" + JSFGeneratorUtil.createManagedBeanName(modelObject.getModelClassName() + CONVERSION_SUFFIX) + "\")\n");

		if (project.isJakartaEEApplication())
			b.append("@RequestScoped\n");
		else
			b.append("@RequestScope\n");

		b.append("public class " + modelObject.getModelClassName() + CONVERSION_SUFFIX);
		b.append(" implements Converter<" + modelObject.getModelClassName() + ">");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		addPrivateConstant(JavaType.STRING, CONSTANT_NBSP, "\"&nbsp;\"").create();

		if (isMandated)
			addPrivateField(USER_SESSION_BEAN_TYPE, USER_SESSION_BEAN).inject().create();

		if (displayAttr != null)
			new ServiceDeclarationGenerator(this, boundaryBean).addField(false);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final DTOBeanAttribute pkAttr = modelObject.getPKAttribute();
		var identifier = modelObject.getModelClassName()
				+ " getAsObject(FacesContext facesContext, UIComponent component, String submittedValue)";

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.faces.convert.Converter#");
		b.append("getAsObject(jakarta.faces.context.FacesContext, jakarta.faces.component.UIComponent, java.lang.String)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("if(submittedValue == null || submittedValue.isEmpty() || submittedValue.equals(" + CONSTANT_NBSP + "))\n");
		b.append("return null;\n\n");

		if (displayAttr == null) {
			b.append("return new " + modelObject.getModelClassName() + "(");
			b.append(pkAttr.getDomainAttribute().convertFromString("submittedValue"));
			b.append(");\n");
		}
		else {
			final Optional<BoundaryMethod> boundaryMethod = boundaryBean.getBoundaryMethods().stream()
					.filter(method -> method.getMethodType() == BoundaryMethodTypeEnumeration.SEARCH_BY_FILTER)
					.filter(method -> method.getReturnType().equals(modelObject)).findFirst();

			if (boundaryMethod.isPresent()) {
				b.append("try\n");
				b.append("{\n");
				b.append("return ");

				new ServiceInvocationGenerator(boundaryMethod.get(), modelObject, b).addInvocation(true, "submittedValue");

				b.append(".stream().findFirst().orElse(null);\n");
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while performing object conversion by using the provided input '{}'!", "e", "submittedValue");

				b.append("}\n\n");
			}
			else
				b.append("// An appropriate boundary method of type SEARCH_BY_FILTER was not found!\n");

			b.append("return null;\n");
		}

		b.append("}\n\n");

		addMethod(identifier, b.toString());

		identifier = "String getAsString(FacesContext facesContext, UIComponent component, " + modelObject.getModelClassName()
				+ " value)";

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see jakarta.faces.convert.Converter#");
		b.append("getAsString(jakarta.faces.context.FacesContext, jakarta.faces.component.UIComponent, java.lang.Object)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public " + identifier + "\n");
		b.append("{\n");
		b.append("if(value == null)\n");
		b.append("return null;\n\n");
		b.append("return ");

		if (displayAttr == null)
			b.append(pkAttr.getDomainAttribute().convertToString("value." + pkAttr.getModelGetterName()));
		else
			b.append("value." + displayAttr.getModelGetterName());

		b.append(";\n");
		b.append("}\n\n");

		addMethod(identifier, b.toString());
	}

}
