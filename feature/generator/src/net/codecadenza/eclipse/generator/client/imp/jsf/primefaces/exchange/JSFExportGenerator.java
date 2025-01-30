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
package net.codecadenza.eclipse.generator.client.imp.jsf.primefaces.exchange;

import static net.codecadenza.eclipse.shared.Constants.ACTION_PREFIX;

import java.util.regex.Pattern;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.util.JavaBeanHelper;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for data export operations
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JSFExportGenerator {
	private static final Pattern ACTION_PREFIX_PATTERN = Pattern.compile(ACTION_PREFIX);

	private final AbstractJavaSourceGenerator generator;
	private final EList<FormAction> actions = new BasicEList<>();

	/**
	 * Constructor
	 * @param generator
	 * @param form
	 */
	public JSFExportGenerator(AbstractJavaSourceGenerator generator, Form form) {
		this.generator = generator;

		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.DOWNLOAD_EXPORT)
				this.actions.add(a);
	}

	/**
	 * Constructor
	 * @param generator
	 * @param panel
	 */
	public JSFExportGenerator(AbstractJavaSourceGenerator generator, FormPanel panel) {
		this.generator = generator;

		for (final FormAction a : panel.getActions())
			if (a.getType() == ActionType.DOWNLOAD_EXPORT)
				this.actions.add(a);
	}

	/**
	 * Add all necessary imports
	 */
	public void addImports() {
		actions.forEach(action -> {
			final var exchangeMethod = (DataExchangeMethod) action.getBoundaryMethod().getServiceMethod();

			if (exchangeMethod.returnsPath() || exchangeMethod.returnsContent()) {
				generator.importPackage("org.primefaces.model");
				generator.importPackage("java.io");
			}
		});
	}

	/**
	 * @param formAction
	 * @param selItemName
	 */
	public void addExportDataMethod(FormAction formAction, String selItemName) {
		final var b = new StringBuilder();
		final BoundaryBean boundaryBean = formAction.getBoundaryMethod().getBoundaryBean();
		final var exchangeMethod = (DataExchangeMethod) formAction.getBoundaryMethod().getServiceMethod();
		final boolean returnsContent = exchangeMethod.returnsContent();
		final boolean returnsPath = exchangeMethod.returnsPath();
		final var invocationGenerator = new ServiceInvocationGenerator(formAction.getBoundaryMethod(), b);
		String methodSignature;

		if (returnsContent || returnsPath) {
			String propertyName = formAction.getName();

			if (formAction.getName().startsWith(ACTION_PREFIX))
				propertyName = ACTION_PREFIX_PATTERN.matcher(propertyName).replaceFirst("");

			methodSignature = "StreamedContent " + JavaBeanHelper.getGetterName(propertyName, false);
		}
		else
			methodSignature = JavaType.VOID + " " + formAction.getBoundaryMethod().getName() + "()";

		b.append("/**\n");
		b.append(" * " + formAction.getDescription() + "\n");

		if (returnsContent || returnsPath)
			b.append(" * @return the download stream that contains the file content\n");

		b.append(" */\n");
		b.append(generator.getAnnotationForGeneratedElement());

		if (returnsPath) {
			// The FileInputStream must not be closed! Otherwise, the content cannot be streamed!
			b.append("@SuppressWarnings(\"resource\")\n");
		}

		b.append("public " + methodSignature + "\n");
		b.append("{\n");
		b.append("try\n");
		b.append("{\n");

		generator.addDebugLog(b, "Start data export");

		b.append("\n");

		if (returnsContent)
			b.append("final String content = ");

		if (returnsPath)
			b.append("final String path = ");

		if (exchangeMethod.getSingleObjectFilterParam() != null)
			invocationGenerator.addInvocation(selItemName);
		else
			invocationGenerator.addInvocation();

		b.append("\n");

		if (returnsContent) {
			b.append("final var inputStream = new ByteArrayInputStream(content.getBytes());\n");
			b.append("return DefaultStreamedContent.builder().name(");
			b.append("\"" + boundaryBean.getDomainObject().getName() + ".");
			b.append(exchangeMethod.getDefaultFileExtension());
			b.append("\").stream(() -> inputStream).build();\n");
		}

		if (returnsPath) {
			b.append("final String fileName = new File(path).getName();\n");
			b.append("final var inputStream = new FileInputStream(path);\n\n");
			b.append("return DefaultStreamedContent.builder().name(fileName).stream(() -> inputStream).build();\n");
		}

		if (!returnsContent && !returnsPath)
			b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_INFO, OPERATION_EXPORT_OK);\n");

		b.append("}\n");
		b.append("catch (final Exception e)\n");
		b.append("{\n");

		generator.addErrorLog(b, "Error while performing data export operation!", "e");

		b.append("\n");
		b.append("MessageUtil.sendFacesMessage(bundle, FacesMessage.SEVERITY_ERROR, OPERATION_EXPORT_FAIL, e);\n");

		if (returnsContent || returnsPath)
			b.append("return null;\n");

		b.append("}\n");
		b.append("}\n\n");

		generator.addMethod(methodSignature, b.toString());
	}

	/**
	 * Add methods for all data export actions
	 * @param selItemName
	 */
	public void addDataExportMethods(String selItemName) {
		actions.forEach(action -> addExportDataMethod(action, selItemName));
	}

}
