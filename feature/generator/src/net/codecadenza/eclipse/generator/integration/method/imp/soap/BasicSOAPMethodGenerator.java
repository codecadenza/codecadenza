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
package net.codecadenza.eclipse.generator.integration.method.imp.soap;

import static net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil.LIST_INSTANCE_NAME;
import static net.codecadenza.eclipse.generator.integration.method.imp.util.AbstractIntegrationMethodUtil.createServiceMethodInvocation;

import java.util.HashSet;
import java.util.Set;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator;
import net.codecadenza.eclipse.model.integration.SOAPIntegrationMethod;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;

/**
 * <p>
 * Base class for SOAP integration method generators
 * </p>
 * <p>
 * Copyright 2016 (C) Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BasicSOAPMethodGenerator extends AbstractIntegrationMethodGenerator {
	protected final SOAPIntegrationMethod soapMethod;
	protected final boolean addTryCatchBlock;

	/**
	 * Constructor
	 * @param soapMethod
	 * @param parentGenerator
	 */
	public BasicSOAPMethodGenerator(SOAPIntegrationMethod soapMethod, AbstractJavaSourceGenerator parentGenerator) {
		super(soapMethod, parentGenerator);

		this.soapMethod = soapMethod;
		this.addTryCatchBlock = (addTransaction() && project.isSpringBootApplication()) || soapMethod.isStartNewThread();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createComment()
	 */
	@Override
	public String createComment() {
		final var b = new StringBuilder();
		final String returnComment = createReturnComment(method);

		b.append(method.generateBeginOfJavadocComment());

		method.getIntegrationParameters().forEach(param -> {
			b.append(" * @param " + param.getName());

			if (param.getComment() != null)
				b.append(" " + param.getComment());

			b.append("\n");
		});

		if (!returnComment.isEmpty())
			b.append(" * @return " + returnComment + "\n");

		b.append(" * @throws WebServiceException due to a network problem or if the call failed in the backend\n");
		b.append(" */\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createMethod()
	 */
	@Override
	public String createMethod() {
		final var b = new StringBuilder();

		if (addTransaction() && !project.isSpringBootApplication())
			b.append("@Transactional\n");

		b.append("@Override\n");
		b.append(parentGenerator.getAnnotationForGeneratedElement());
		b.append(getMethodSignature(true, false, true));
		b.append("\n{\n");
		b.append(createMethodBody());
		b.append("}\n\n");

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createMethodBody()
	 */
	@Override
	public String createMethodBody() {
		final var b = new StringBuilder();

		if (soapMethod.isStartNewThread()) {
			String returnType = method.getReturnType().getWrapperTypeName();

			if (soapMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
				returnType = returnType + " []";

			b.append("final Future<" + returnType + "> result = " + EXECUTOR_SERVICE_NAME + ".submit(() ->");
			b.append("{\n");

			if (addTransaction() && project.isSpringBootApplication()) {
				b.append("final var def = new DefaultTransactionDefinition();\n");
				b.append("final TransactionStatus status = transactionManager.getTransaction(def);\n\n");
				b.append("try\n");
				b.append("{\n");
			}

			b.append(createMethodLogic());

			if (soapMethod.getReturnType().isVoid())
				b.append("return null;\n");

			if (addTransaction() && project.isSpringBootApplication()) {
				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");
				b.append("transactionManager.rollback(status);\n\n");
				b.append("throw new " + getExceptionName() + "(ExceptionHelper.getRootCause(e).getMessage());\n");
				b.append("}\n");
			}

			b.append("});\n\n");
			b.append("try\n");
			b.append("{\n");

			if (!soapMethod.getReturnType().isVoid())
				b.append("return ");

			b.append("result.get(DEFAULT_TIMEOUT_VALUE, DEFAULT_TIMEOUT_UNIT);\n");
			b.append("}\n");
			b.append("catch (final InterruptedException e)\n");
			b.append("{\n");
			b.append("Thread.currentThread().interrupt();\n\n");
			b.append("throw new " + getExceptionName() + "(ExceptionHelper.getRootCause(e).getMessage());\n");
		}
		else {
			if (addTransaction() && project.isSpringBootApplication()) {
				b.append("final var def = new DefaultTransactionDefinition();\n");
				b.append("final TransactionStatus status = transactionManager.getTransaction(def);\n\n");
			}

			if (addTryCatchBlock) {
				b.append("try\n");
				b.append("{\n");
			}

			b.append(createMethodLogic());
		}

		if (addTryCatchBlock) {
			b.append("}\n");
			b.append("catch (final Exception e)\n");
			b.append("{\n");

			if (!soapMethod.isStartNewThread() && addTransaction() && project.isSpringBootApplication())
				b.append("transactionManager.rollback(status);\n\n");

			b.append("throw new " + getExceptionName() + "(ExceptionHelper.getRootCause(e).getMessage());\n");
			b.append("}\n");
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#createMethodLogic()
	 */
	@Override
	public String createMethodLogic() {
		final var b = new StringBuilder();
		final String facadeFragment = createFacadeFragment();

		if (facadeFragment.isEmpty()) {
			if (!soapMethod.getReturnType().isVoid())
				if (soapMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
					b.append("final List<" + soapMethod.getReturnType().getName() + "> " + LIST_INSTANCE_NAME + " = ");
				else if (addTransaction() && project.isSpringBootApplication())
					b.append("final " + soapMethod.getReturnType().getName() + " invocationResult " + " = ");
				else
					b.append("return ");

			b.append(createServiceMethodInvocation(soapMethod, getServiceName()));

			if (addTransaction() && project.isSpringBootApplication()) {
				b.append("\n");
				b.append("transactionManager.commit(status);\n");
			}

			if (!soapMethod.getReturnType().isVoid()) {
				if (soapMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE) {
					b.append("\nreturn " + LIST_INSTANCE_NAME + ".stream().toArray(size -> new ");
					b.append(soapMethod.getReturnType().getName() + "[size]);\n");
				}
				else if (addTransaction() && project.isSpringBootApplication())
					b.append("\nreturn invocationResult;\n");
			}
		}
		else {
			b.append(facadeFragment);

			if (addTransaction() && project.isSpringBootApplication()) {
				b.append("\n");
				b.append("transactionManager.commit(status);\n");
			}

			if (!soapMethod.getReturnType().isVoid()) {
				b.append("\nreturn " + getReturnObjectName());

				if (soapMethod.getReturnTypeModifier() != JavaTypeModifierEnumeration.NONE)
					b.append(".stream().toArray(size -> new " + soapMethod.getReturnType().getName() + "[size])");

				b.append(";\n");
			}
		}

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getMethodSignature(boolean,
	 * boolean, boolean)
	 */
	@Override
	public String getMethodSignature(boolean addQualifier, boolean addAnnotations, boolean generateFullSignature) {
		final var b = new StringBuilder();

		if (addAnnotations) {
			final String returnValue = soapMethod.getReturnValueName();
			final String returnValuePart = soapMethod.getReturnValuePartName();
			final boolean addWebResultAnnotation = ((returnValue != null && !returnValue.isEmpty())
					|| (returnValuePart != null && !returnValuePart.isEmpty()));

			if (soapMethod.getOperationName() != null && !soapMethod.getOperationName().isEmpty())
				b.append("@WebMethod(operationName = \"" + soapMethod.getOperationName() + "\")\n");

			if (addWebResultAnnotation) {
				boolean addReturnValueName = false;

				b.append("@WebResult(");

				if (returnValue != null && !returnValue.isEmpty()) {
					b.append("name = \"" + soapMethod.getReturnValueName() + "\"");
					addReturnValueName = true;
				}

				if (returnValuePart != null && !returnValuePart.isEmpty()) {
					if (addReturnValueName)
						b.append(", ");

					b.append("partName = \"" + soapMethod.getReturnValuePartName() + "\"");
				}

				b.append(")\n");
			}
		}

		b.append(super.getMethodSignature(addQualifier, addAnnotations, generateFullSignature));

		return b.toString();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getImports()
	 */
	@Override
	public Set<String> getImports() {
		final var imports = new HashSet<>(super.getImports());

		if (addTryCatchBlock) {
			imports.add("import net.codecadenza.runtime.exception.*;");
			imports.add("import net.codecadenza.runtime.transport.*;");
		}

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#getInterfaceImports()
	 */
	@Override
	public Set<String> getInterfaceImports() {
		final Set<String> imports = super.getInterfaceImports();
		final String returnValue = soapMethod.getReturnValueName();
		final String returnValuePart = soapMethod.getReturnValuePartName();
		final boolean addWebMethodAnnotation = soapMethod.getOperationName() != null && !soapMethod.getOperationName().isEmpty();
		final boolean addWebResultAnnotation = ((returnValue != null && !returnValue.isEmpty())
				|| (returnValuePart != null && !returnValuePart.isEmpty()));

		if (soapMethod.isAddParameterAnnotations() || addWebMethodAnnotation || addWebResultAnnotation)
			imports.add("import jakarta.jws.*;");

		return imports;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.integration.method.AbstractIntegrationMethodGenerator#returnList()
	 */
	@Override
	protected boolean returnList() {
		// In some configurations (e.g. @SOAPBinding(parameterStyle = ParameterStyle.BARE, style = Style.RPC) a container
		// uses JAXB in order to convert parameters and values! In this case a method must not return a list! Rather, an
		// array must be returned!
		return false;
	}

}
