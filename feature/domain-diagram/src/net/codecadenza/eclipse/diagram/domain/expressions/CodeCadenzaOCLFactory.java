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
package net.codecadenza.eclipse.diagram.domain.expressions;

import java.lang.ref.WeakReference;
import java.util.Collections;
import java.util.Iterator;
import java.util.Map;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.ecore.EClassifier;
import org.eclipse.ocl.Environment;
import org.eclipse.ocl.EvaluationEnvironment;
import org.eclipse.ocl.ParserException;
import org.eclipse.ocl.Query;
import org.eclipse.ocl.ecore.EcoreFactory;
import org.eclipse.ocl.expressions.OCLExpression;
import org.eclipse.ocl.expressions.OperationCallExp;
import org.eclipse.ocl.expressions.Variable;
import org.eclipse.ocl.helper.OCLHelper;
import org.eclipse.ocl.utilities.AbstractVisitor;
import org.eclipse.ocl.utilities.PredefinedType;

/**
 * <p>
 * OCL factory
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
@SuppressWarnings("rawtypes")
public class CodeCadenzaOCLFactory {
	/**
	 * Prevent instantiation
	 */
	private CodeCadenzaOCLFactory() {
	}

	/**
	 * @param body
	 * @param context
	 * @param environment
	 * @return a new expression
	 */
	public static CodeCadenzaAbstractExpression getExpression(String body, EClassifier context, Map environment) {
		return new Expression(body, context, environment);
	}

	/**
	 * @param body
	 * @param context
	 * @return an expression
	 */
	public static CodeCadenzaAbstractExpression getExpression(String body, EClassifier context) {
		return getExpression(body, context, Collections.emptyMap());
	}

	private static class Expression extends CodeCadenzaAbstractExpression {
		private WeakReference queryRef;
		private final org.eclipse.ocl.ecore.OCL oclInstance;

		/**
		 * @param body
		 * @param context
		 * @param environment
		 */
		public Expression(String body, EClassifier context, Map environment) {
			super(body, context);

			oclInstance = org.eclipse.ocl.ecore.OCL.newInstance();
			initCustomEnv(oclInstance.getEnvironment(), environment);
		}

		/**
		 * @return a query
		 */
		@SuppressWarnings("unchecked")
		protected Query getQuery() {
			Query oclQuery = null;

			if (this.queryRef != null)
				oclQuery = (Query) this.queryRef.get();

			if (oclQuery == null) {
				final OCLHelper oclHelper = oclInstance.createOCLHelper();
				oclHelper.setContext(context());

				try {
					final OCLExpression oclExpression = oclHelper.createQuery(body());
					oclQuery = oclInstance.createQuery(oclExpression);
					this.queryRef = new WeakReference(oclQuery);
					setStatus(IStatus.OK, null, null);
				}
				catch (final ParserException e) {
					setStatus(IStatus.ERROR, e.getMessage(), e);
				}
			}

			return oclQuery;
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.diagram.domain.expressions.CodeCadenzaAbstractExpression#doEvaluate(java.lang.Object, java.util.Map)
		 */
		@Override
		protected Object doEvaluate(Object context, Map env) {
			final Query oclQuery = getQuery();

			if (oclQuery == null)
				return null;

			final EvaluationEnvironment evalEnv = oclQuery.getEvaluationEnvironment();

			// Initialize the environment
			for (final Iterator it = env.entrySet().iterator(); it.hasNext();) {
				final Map.Entry nextEntry = (Map.Entry) it.next();
				evalEnv.replace((String) nextEntry.getKey(), nextEntry.getValue());
			}

			try {
				initExtentMap(context);
				final Object result = oclQuery.evaluate(context);

				return (result != oclInstance.getEnvironment().getOCLStandardLibrary().getOclInvalid()) ? result : null;
			}
			finally {
				evalEnv.clear();
				oclQuery.getExtentMap().clear();
			}
		}

		/**
		 * @param context
		 */
		@SuppressWarnings("unchecked")
		private void initExtentMap(Object context) {
			if (!getStatus().isOK() || context == null)
				return;

			final Query queryToInit = getQuery();
			final Object extentContext = context;

			queryToInit.getExtentMap().clear();

			if (queryToInit.queryText() != null && queryToInit.queryText().contains(PredefinedType.ALL_INSTANCES_NAME)) {
				final var visitior = new AbstractVisitor() {
					private boolean usesAllInstances;

					/*
					 * (non-Javadoc)
					 * @see org.eclipse.ocl.utilities.AbstractVisitor#visitOperationCallExp(org.eclipse.ocl.expressions.OperationCallExp)
					 */
					@Override
					public Object visitOperationCallExp(OperationCallExp oc) {
						if (!usesAllInstances) {
							usesAllInstances = PredefinedType.ALL_INSTANCES == oc.getOperationCode();

							if (usesAllInstances)
								queryToInit.getExtentMap().putAll(oclInstance.getEvaluationEnvironment().createExtentMap(extentContext));
						}

						return super.visitOperationCallExp(oc);
					}
				};

				queryToInit.getExpression().accept(visitior);
			}
		}

		/**
		 * @param ecoreEnv
		 * @param environment
		 */
		@SuppressWarnings("unchecked")
		private static void initCustomEnv(Environment ecoreEnv, Map environment) {
			for (final Iterator it = environment.keySet().iterator(); it.hasNext();) {
				final var varName = (String) it.next();
				final var varType = (EClassifier) environment.get(varName);
				ecoreEnv.addElement(varName, createVar(ecoreEnv, varName, varType), false);
			}
		}

		/**
		 * @param ecoreEnv
		 * @param name
		 * @param type
		 * @return a variable
		 */
		@SuppressWarnings("unchecked")
		private static Variable createVar(Environment ecoreEnv, String name, EClassifier type) {
			final Variable variable = EcoreFactory.eINSTANCE.createVariable();
			variable.setName(name);
			variable.setType(ecoreEnv.getUMLReflection().getOCLType(type));

			return variable;
		}
	}

}
